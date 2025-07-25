/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.rw

import scala.annotation.nowarn

import fabric.*
import fabric.rw.*
import fabric.define.*

import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*

import scala.util.Try

import scala.collection.immutable.VectorMap

@nowarn()
trait CompileRW {
  inline final def derived[T <: Product](using inline T: Mirror.ProductOf[T], ct: ClassTag[T]): RW[T] = gen[T]

  inline def gen[T <: Product](using Mirror.ProductOf[T], ClassTag[T]): RW[T] =
    ${ CompileRW.genMacro[T] }

  inline def genR[T <: Product](using Mirror.ProductOf[T]): Reader[T] = new ClassR[T] {
    override protected def t2Map(t: T): Map[String, Json] = toMap(t)
  }

  inline def genW[T <: Product](using Mirror.ProductOf[T]): Writer[T] =
    ${ CompileRW.genWMacro[T] }

  // Enum support
  inline def genEnum[T](using m: Mirror.SumOf[T]): RW[T] = new RW[T] {
    override def read(value: T): Json = Str(enumName(value))

    override def write(json: Json): T = json match {
      case Str(name, _) => fromName[T](name)
      case _ => throw RWException(s"Expected string for enum, got: $json")
    }

    override def definition: DefType = DefType.Str
  }

  inline def genEnumR[T](using m: Mirror.SumOf[T]): Reader[T] = new Reader[T] {
    override def read(value: T): Json = Str(enumName(value))
  }

  inline def genEnumW[T](using m: Mirror.SumOf[T]): Writer[T] = new Writer[T] {
    override def write(json: Json): T = json match {
      case Str(name, _) => fromName[T](name)
      case _ => throw new RWException(s"Expected string for enum, got: $json")
    }
  }

  inline def enumName[T](value: T)(using m: Mirror.SumOf[T]): String =
    value.asInstanceOf[scala.reflect.Enum].productPrefix

  inline def fromName[T](name: String)(using m: Mirror.SumOf[T]): T =
    ${ CompileRW.fromNameImpl[T]('name) }

  inline def toClassName[T](using ct: ClassTag[T]): Option[String] =
    Some(ct.runtimeClass.getName.replace("$", "."))

  inline def toDefinition[T <: Product](using p: Mirror.ProductOf[T], ct: ClassTag[T]): DefType = {
    DefType.Obj(toDefinitionElems[T, p.MirroredElemTypes, p.MirroredElemLabels](0), toClassName[T])
  }

  inline def toDefinitionElems[A <: Product, T <: Tuple, L <: Tuple](index: Int): Map[String, DefType] = {
    inline erasedValue[T] match {
      case _: (hd *: tl) => {
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val defaults = getDefaultParams[A]
            val rw = summonInline[RW[hd]]
            val d = if (defaults.contains(hdLabelValue)) rw.definition.opt else rw.definition
            VectorMap(hdLabelValue -> d) ++ toDefinitionElems[A, tl, tlLabels](index + 1)
          case EmptyTuple => sys.error("Not possible")
        }
      }
      case EmptyTuple => Map.empty
    }
  }

  inline def toMap[T <: Product](t: T)(using p: Mirror.ProductOf[T]): Map[String, Json] = {
    toMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](t, 0)
  }

  inline def toMapElems[A <: Product, T <: Tuple, L <: Tuple](a: A, index: Int): Map[String, Json] = {
    inline erasedValue[T] match {
      case _: (hd *: tl) => {
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValue = a.productElement(index).asInstanceOf[hd]
            val hdReader = summonInline[Reader[hd]]
            val value = hdReader.read(hdValue)
            VectorMap(hdLabelValue -> value) ++ toMapElems[A, tl, tlLabels](a, index + 1)
          case EmptyTuple => sys.error("Not possible")
        }
      }
      case EmptyTuple => Map.empty
    }
  }

  inline def fromMap[T <: Product](map: Map[String, Json])(using p: Mirror.ProductOf[T]): T = {
    inline val size = constValue[Tuple.Size[p.MirroredElemTypes]]
    val defaults = getDefaultParams[T]
    val arr = new Array[Any](size)
    fromMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](map, 0, arr, defaults)
    val product: Product = new Product {
      override def canEqual(that: Any): Boolean = true
      override def productArity: Int = arr.size
      override def productElement(n: Int): Any = arr(n)
    }
    p.fromProduct(product)
  }

  inline def fromMapElems[A <: Product, T <: Tuple, L <: Tuple](map: Map[String, Json], index: Int, arr: Array[Any], defaults: Map[String, Any]): Unit = {
    val isJsonWrapper = inline erasedValue[A] match {
      case _: JsonWrapper => true
      case _ => false
    }
    inline erasedValue[T] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue: String = constValue[hdLabel].asInstanceOf[String]
            val hdValueOption: Option[Json] = map.get(hdLabelValue)
            val hdWritable: Writer[hd] = summonInline[Writer[hd]]
            def defaultAlternative = if (hdLabelValue == "json" && isJsonWrapper) {
              Obj(map)
            } else {
              inline erasedValue[hd] match {
                case _: Option[optHd] => None
                case _ => throw RWException(s"Unable to find field ${getClassName[A]}.$hdLabelValue (and no defaults set) in ${Obj(map)}")
              }
            }
            lazy val default = Try(defaults.getOrElse(hdLabelValue, defaultAlternative)).toOption
            val value = hdValueOption.map {
              case Null if default.nonEmpty => default.get
              case json => hdWritable.write(json)
            }.getOrElse(default.get)
            arr(index) = value
            fromMapElems[A, tl, tlLabels](map, index + 1, arr, defaults)
          case EmptyTuple => sys.error("Not possible")
        }
      case EmptyTuple => // Finished
    }
  }

  inline def getDefaultParams[T]: Map[String, AnyRef] = ${ CompileRW.getDefaultParmasImpl[T] }

  inline def getClassName[T]: String = ${ CompileRW.getClassNameImpl[T] }
}

object CompileRW extends CompileRW {
  def getDefaultParmasImpl[T](using Quotes, Type[T]): Expr[Map[String, AnyRef]] = {
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
          yield p.name
      val namesExpr: Expr[List[String]] = Expr.ofList(names.map(Expr(_)))

      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Ref] =
        for case deff @ DefDef(name, _, _, _) <- body
            if name.startsWith("$lessinit$greater$default")
        yield Ref(deff.symbol)
      val identsExpr: Expr[List[Any]] =
        Expr.ofList(idents.map(_.asExpr))

      '{ $namesExpr.zip($identsExpr.map(_.asInstanceOf[AnyRef])).toMap }
    } else {
      '{ Map.empty }
    }
  }

  def getClassNameImpl[T](using Quotes, Type[T]): Expr[String] = {
    import quotes.reflect._

    Expr(TypeTree.of[T].symbol.companionClass.fullName)
  }

  def fromNameImpl[T: Type](name: Expr[String])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Get the companion module
    val companion = typeSymbol.companionModule

    // For enums, use the valueOf method which is standard
    val valueOfMethod = companion.methodMember("valueOf").headOption.getOrElse {
      report.errorAndAbort(s"No valueOf method found for enum ${typeSymbol.name}")
    }

    Apply(
      Select(Ref(companion), valueOfMethod),
      List(name.asTerm)
    ).asExprOf[T]
  }

  def genMacro[T <: Product: Type](using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val mirror = Expr.summon[Mirror.ProductOf[T]].getOrElse {
      report.errorAndAbort(s"No Mirror.ProductOf found for ${Type.show[T]}")
    }
    val ct = Expr.summon[ClassTag[T]].getOrElse {
      report.errorAndAbort(s"No ClassTag found for ${Type.show[T]}")
    }

    '{
      new ClassRW[T] {
        override protected def t2Map(t: T): Map[String, Json] = CompileRW.toMap(t)(using $mirror)

        override protected def map2T(map: Map[String, Json]): T = {
          ${ generateDirectConstructor[T]('{map}) }
        }

        override def definition: DefType = CompileRW.toDefinition[T](using $mirror, $ct)
      }
    }
  }

  def genWMacro[T <: Product: Type](using Quotes): Expr[Writer[T]] = {
    '{
      new ClassW[T] {
        override protected def map2T(map: Map[String, Json]): T = {
          ${ generateDirectConstructor[T]('{map}) }
        }
      }
    }
  }

  private def generateDirectConstructor[T <: Product: Type](map: Expr[Map[String, Json]])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    if (!typeSymbol.isClassDef || !typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${typeSymbol.name} is not a case class")
    }

    val fields = typeSymbol.caseFields
    val companionModule = typeSymbol.companionModule

    // Check if T is a JsonWrapper
    val isJsonWrapperType = tpe <:< TypeRepr.of[JsonWrapper]

    // Get default values
    val defaultsExpr = getDefaultParmasImpl[T]

    // Generate field extraction expressions
    val fieldExprs = fields.map { field =>
      val fieldName = field.name
      val fieldTypeRepr = tpe.memberType(field)

      fieldTypeRepr.asType match {
        case '[ft] =>
          val writer = Expr.summon[Writer[ft]].getOrElse {
            report.errorAndAbort(s"No Writer found for field $fieldName of type ${Type.show[ft]}")
          }

          val isOptional = fieldTypeRepr <:< TypeRepr.of[Option[?]]
          val fieldNameStr = Expr(fieldName)
          val isJsonWrapperExpr = Expr(isJsonWrapperType)
          val classNameExpr = Expr(typeSymbol.fullName)

          '{
            val defaults = $defaultsExpr
            val jsonOpt = $map.get($fieldNameStr)
            val defaultOpt = defaults.get($fieldNameStr)

            jsonOpt match {
              case Some(json) =>
                json match {
                  case fabric.Null if defaultOpt.isDefined => defaultOpt.get.asInstanceOf[ft]
                  case _ => $writer.write(json)
                }
              case None =>
                if ($fieldNameStr == "json" && $isJsonWrapperExpr) {
                  $writer.write(fabric.Obj($map))
                } else {
                  defaultOpt match {
                    case Some(defaultValue) => defaultValue.asInstanceOf[ft]
                    case None =>
                      ${
                        if (isOptional) {
                          '{ None.asInstanceOf[ft] }
                        } else {
                          '{ throw fabric.rw.RWException(s"Unable to find field ${$classNameExpr}.${$fieldNameStr} (and no defaults set) in ${fabric.Obj($map)}") }
                        }
                      }
                  }
                }
            }
          }
      }
    }

    // For case classes, we need to use the companion object's apply method
    // Get the companion object reference based on the type
    val companionRef = tpe match {
      case AppliedType(tycon, typeArgs) =>
        // For generic types like OrganizationDetail[Org], we need to get the companion
        // and potentially apply type arguments
        Ref(companionModule)
      case _ =>
        // For non-generic types
        Ref(companionModule)
    }

    // Find the apply method in the companion
    val applyMethods = companionModule.methodMember("apply")

    // Find the correct apply method by checking parameter count
    val applyMethod = applyMethods.find { method =>
      method.paramSymss match {
        case typeParams :: valueParams :: Nil if typeParams.forall(_.isType) =>
          // Generic apply method with type parameters
          valueParams.length == fields.length
        case valueParams :: Nil if !valueParams.exists(_.isType) =>
          // Non-generic apply method
          valueParams.length == fields.length
        case _ => false
      }
    }.getOrElse {
      report.errorAndAbort(s"No suitable apply method found in companion object of ${typeSymbol.name}")
    }

    // Create the method call
    val methodCall = Select(companionRef, applyMethod)

    // Apply type arguments if needed
    val appliedMethod = tpe match {
      case AppliedType(_, typeArgs) if applyMethod.paramSymss.headOption.exists(_.forall(_.isType)) =>
        // The apply method has type parameters, apply them
        val typeTreeArgs = typeArgs.map { arg =>
          arg.asType match {
            case '[t] => TypeTree.of[t]
          }
        }
        TypeApply(methodCall, typeTreeArgs)
      case _ =>
        // No type parameters needed
        methodCall
    }

    // Apply the value arguments
    val constructorCall = Apply(appliedMethod, fieldExprs.map(_.asTerm))

    constructorCall.asExprOf[T]
  }
}