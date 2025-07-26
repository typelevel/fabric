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
  inline final def derived[T](using inline T: Mirror.ProductOf[T], ct: ClassTag[T]): RW[T] = gen[T]

  inline def gen[T](using ct: ClassTag[T]): RW[T] = ${ CompileRW.genDispatch[T] }

  inline def genR[T](using Mirror.ProductOf[T]): Reader[T] = new ClassR[T] {
    override protected def t2Map(t: T): Map[String, Json] = toMap(t)
  }

  inline def genW[T](using Mirror.ProductOf[T]): Writer[T] =
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

  // Sealed trait support
  inline def genSealedTrait[T](using m: Mirror.SumOf[T]): RW[T] = new RW[T] {
    private val typeField = "_type"
    private lazy val childRWs = getSealedTraitChildren[T, m.MirroredElemTypes]

    override def read(value: T): Json = {
      val typeName = value.getClass.getSimpleName.stripSuffix("$")
      val (_, rw) = childRWs.find(_._1 == typeName).getOrElse {
        throw RWException(s"Unknown subtype: $typeName")
      }

      rw.asInstanceOf[RW[T]].read(value) match {
        case obj: Obj => obj.merge(Obj(typeField -> Str(typeName)))
        case other => Obj(typeField -> Str(typeName), "value" -> other)
      }
    }

    override def write(json: Json): T = json match {
      case obj @ Obj(map) =>
        map.get(typeField) match {
          case Some(Str(typeName, _)) =>
            childRWs.find(_._1 == typeName) match {
              case Some((_, rw)) =>
                val cleanedMap = map - typeField
                val cleanedJson = if (cleanedMap.isEmpty && map.size == 2 && map.contains("value")) {
                  map("value")
                } else {
                  Obj(cleanedMap)
                }
                rw.asInstanceOf[RW[T]].write(cleanedJson)
              case None =>
                throw RWException(s"Unknown type discriminator: $typeName")
            }
          case _ =>
            throw RWException(s"Missing or invalid $typeField field in JSON")
        }
      case _ =>
        throw RWException(s"Expected JSON object for sealed trait, got: $json")
    }

    override def definition: DefType = {
      val childDefs = childRWs.map { case (name, rw) =>
        name -> rw.definition
      }.toMap
      DefType.Poly(childDefs, Some(getSimpleTypeName[T]))
    }
  }

  inline def getSealedTraitChildren[T, Types <: Tuple]: List[(String, RW[_])] = {
    inline erasedValue[Types] match {
      case _: (head *: tail) =>
        val rw = summonInline[RW[head]]
        val name = getSimpleTypeName[head]
        (name, rw) :: getSealedTraitChildren[T, tail]
      case _: EmptyTuple =>
        Nil
    }
  }

  inline def getSimpleTypeName[T]: String = ${ CompileRW.getSimpleTypeNameImpl[T] }

  // Helper for case objects
  inline def singleton[T](instance: T): RW[T] = new RW[T] {
    override def read(value: T): Json = Obj()
    override def write(json: Json): T = instance
    override def definition: DefType = DefType.Obj(Map.empty, Some(getSimpleTypeName[T]))
  }

  // Enumeration support for sealed traits with only case objects
  inline def enumeration[T](instances: List[T]): RW[T] = new RW[T] {
    private val nameToInstance = instances.map(i => i.toString -> i).toMap
    private val instanceToName = instances.map(i => i -> i.toString).toMap

    override def read(value: T): Json = Str(instanceToName.getOrElse(value, value.toString))

    override def write(json: Json): T = json match {
      case Str(name, _) =>
        nameToInstance.getOrElse(name, throw RWException(s"Unknown enumeration value: $name"))
      case _ =>
        throw RWException(s"Expected string for enumeration, got: $json")
    }

    override def definition: DefType = DefType.Enum(instanceToName.values.toList.map(str), className = Some(getSimpleTypeName[T]))
  }

  inline def enumName[T](value: T)(using m: Mirror.SumOf[T]): String =
    value.asInstanceOf[scala.reflect.Enum].productPrefix

  inline def fromName[T](name: String)(using m: Mirror.SumOf[T]): T =
    ${ CompileRW.fromNameImpl[T]('name) }

  inline def toClassName[T](using ct: ClassTag[T]): Option[String] =
    Some(ct.runtimeClass.getName.replace("$", "."))

  inline def toDefinition[T](using p: Mirror.ProductOf[T], ct: ClassTag[T]): DefType = {
    DefType.Obj(toDefinitionElems[T, p.MirroredElemTypes, p.MirroredElemLabels](0), toClassName[T])
  }

  inline def toDefinitionElems[A, T <: Tuple, L <: Tuple](index: Int): Map[String, DefType] = {
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

  inline def toMap[T](t: T)(using p: Mirror.ProductOf[T]): Map[String, Json] = {
    toMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](t, 0)
  }

  inline def toMapElems[A, T <: Tuple, L <: Tuple](a: A, index: Int): Map[String, Json] = {
    inline erasedValue[T] match {
      case _: (hd *: tl) => {
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValue = a.asInstanceOf[Product].productElement(index).asInstanceOf[hd]
            val hdReader = summonInline[Reader[hd]]
            val value = hdReader.read(hdValue)
            VectorMap(hdLabelValue -> value) ++ toMapElems[A, tl, tlLabels](a, index + 1)
          case EmptyTuple => sys.error("Not possible")
        }
      }
      case EmptyTuple => Map.empty
    }
  }

  inline def fromMap[T](map: Map[String, Json])(using p: Mirror.ProductOf[T]): T = {
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

  inline def fromMapElems[A, T <: Tuple, L <: Tuple](map: Map[String, Json], index: Int, arr: Array[Any], defaults: Map[String, Any]): Unit = {
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

  def getSimpleTypeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    // Get the simple name without package
    val fullName = symbol.fullName
    val simpleName = fullName.split('.').last

    // Handle case objects by stripping the trailing $
    Expr(simpleName.stripSuffix("$"))
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

  def genDispatch[T: Type](using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Check if it's a sealed trait/abstract class
    if (typeSymbol.flags.is(Flags.Sealed) && (typeSymbol.flags.is(Flags.Trait) || typeSymbol.flags.is(Flags.Abstract))) {
      Expr.summon[Mirror.SumOf[T]] match {
        case Some(mirror) =>
          // Check if all children are case objects
          val childrenTypes = typeSymbol.children.map(_.termRef)
          val allCaseObjects = childrenTypes.forall { childType =>
            val childSymbol = childType.typeSymbol
            childSymbol.flags.is(Flags.Module) && childSymbol.flags.is(Flags.Case)
          }

          if (allCaseObjects) {
            // Generate an enumeration-style RW for case objects
            val instances = childrenTypes.map { childType =>
              Ref(childType.termSymbol).asExprOf[T]
            }
            val instancesList = Expr.ofList(instances)
            '{ RW.enumeration[T]($instancesList) }
          } else {
            // Generate sealed trait RW directly instead of calling inline method
            genSealedTraitMacro[T](mirror)
          }
        case None =>
          report.errorAndAbort(s"No Mirror.SumOf found for sealed trait ${typeSymbol.name}")
      }
    } else if (typeSymbol.flags.is(Flags.Enum)) {
      Expr.summon[Mirror.SumOf[T]] match {
        case Some(mirror) =>
          // Generate enum RW directly instead of calling inline method
          genEnumMacro[T](mirror)
        case None =>
          report.errorAndAbort(s"No Mirror.SumOf found for enum ${typeSymbol.name}")
      }
    } else if (typeSymbol.flags.is(Flags.Case) && typeSymbol.isClassDef) {
      // Handle case classes - call genMacro directly
      genMacro[T]
    } else {
      report.errorAndAbort(s"${typeSymbol.name} is not a supported type for RW generation. It must be a case class, enum, or sealed trait.")
    }
  }

  def genEnumMacro[T: Type](mirror: Expr[Mirror.SumOf[T]])(using Quotes): Expr[RW[T]] = {
    // Generate valueOf lookup at macro time
    val valueOfExpr = generateValueOfLookup[T]()

    '{
      new RW[T] {
        override def read(value: T): Json = Str(value.asInstanceOf[scala.reflect.Enum].productPrefix)

        override def write(json: Json): T = json match {
          case Str(name, _) => $valueOfExpr(name)
          case _ => throw RWException(s"Expected string for enum, got: $json")
        }

        override def definition: DefType = DefType.Str
      }
    }
  }

  private def generateValueOfLookup[T: Type]()(using Quotes): Expr[String => T] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val companion = tpe.typeSymbol.companionModule

    '{ (name: String) =>
      ${
        val valueOfMethod = companion.methodMember("valueOf").headOption.getOrElse {
          report.errorAndAbort(s"No valueOf method found for enum ${tpe.typeSymbol.name}")
        }

        Apply(
          Select(Ref(companion), valueOfMethod),
          List('name.asTerm)
        ).asExprOf[T]
      }
    }
  }

  def genSealedTraitMacro[T: Type](mirror: Expr[Mirror.SumOf[T]])(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    // Extract child types at macro expansion time
    val mirrorTpe = mirror.asTerm.tpe.widen
    val mirroredElemTypes = mirrorTpe.memberType(Symbol.requiredMethod("scala.deriving.Mirror.Sum.MirroredElemTypes")).dealias

    // Generate child RWs at macro expansion time
    val childRWsExpr = generateChildRWs[T](mirroredElemTypes)

    // Compute the simple type name at macro time
    val simpleTypeName = {
      val tpe = TypeRepr.of[T]
      val symbol = tpe.typeSymbol
      val fullName = symbol.fullName
      val simpleName = fullName.split('.').last
      simpleName.stripSuffix("$")
    }
    val simpleTypeNameExpr = Expr(simpleTypeName)

    '{
      new RW[T] {
        private val typeField = "_type"
        private lazy val childRWs = $childRWsExpr

        override def read(value: T): Json = {
          val typeName = value.getClass.getSimpleName.stripSuffix("$")
          val (_, rw) = childRWs.find(_._1 == typeName).getOrElse {
            throw RWException(s"Unknown subtype: $typeName")
          }

          rw.asInstanceOf[RW[T]].read(value) match {
            case obj: Obj => obj.merge(Obj(typeField -> Str(typeName)))
            case other => Obj(typeField -> Str(typeName), "value" -> other)
          }
        }

        override def write(json: Json): T = json match {
          case obj @ Obj(map) =>
            map.get(typeField) match {
              case Some(Str(typeName, _)) =>
                childRWs.find(_._1 == typeName) match {
                  case Some((_, rw)) =>
                    val cleanedMap = map - typeField
                    val cleanedJson = if (cleanedMap.isEmpty && map.size == 2 && map.contains("value")) {
                      map("value")
                    } else {
                      Obj(cleanedMap)
                    }
                    rw.asInstanceOf[RW[T]].write(cleanedJson)
                  case None =>
                    throw RWException(s"Unknown type discriminator: $typeName")
                }
              case _ =>
                throw RWException(s"Missing or invalid $typeField field in JSON")
            }
          case _ =>
            throw RWException(s"Expected JSON object for sealed trait, got: $json")
        }

        override def definition: DefType = {
          val childDefs = childRWs.map { case (name, rw) =>
            name -> rw.definition
          }.toMap
          DefType.Poly(childDefs, Some($simpleTypeNameExpr))
        }
      }
    }
  }

  private def generateChildRWs[T: Type](elemTypes: Any)(using Quotes): Expr[List[(String, RW[_])]] = {
    import quotes.reflect._

    val tpe = elemTypes.asInstanceOf[TypeRepr]

    def extractTypes(tpe: TypeRepr): List[TypeRepr] = tpe match {
      case AppliedType(_, args) if tpe <:< TypeRepr.of[Tuple] =>
        args.flatMap {
          case t if t <:< TypeRepr.of[Tuple] => extractTypes(t)
          case t => List(t)
        }
      case _ => List(tpe)
    }

    val childTypes = extractTypes(tpe)

    val childExprs = childTypes.map { childType =>
      childType.asType match {
        case '[t] =>
          val rw = Expr.summon[RW[t]].getOrElse {
            report.errorAndAbort(s"No RW found for child type ${childType.show}")
          }
          val name = getSimpleTypeNameFromType(childType)
          '{ (${ Expr(name) }, $rw.asInstanceOf[RW[_]]) }
      }
    }

    Expr.ofList(childExprs)
  }

  private def getSimpleTypeNameFromType(tpe: Any)(using Quotes): String = {
    import quotes.reflect._
    val typeRepr = tpe.asInstanceOf[TypeRepr]
    val symbol = typeRepr.typeSymbol
    val fullName = symbol.fullName
    val simpleName = fullName.split('.').last
    simpleName.stripSuffix("$")
  }

  def genMacro[T: Type](using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Verify it's a case class
    if (!typeSymbol.isClassDef || !typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${typeSymbol.name} is not a case class")
    }

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

  def genWMacro[T: Type](using Quotes): Expr[Writer[T]] = {
    '{
      new ClassW[T] {
        override protected def map2T(map: Map[String, Json]): T = {
          ${ generateDirectConstructor[T]('{map}) }
        }
      }
    }
  }

  private def generateDirectConstructor[T: Type](map: Expr[Map[String, Json]])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    if (!typeSymbol.isClassDef || !typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${typeSymbol.name} is not a case class")
    }

    val fields = typeSymbol.caseFields

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

    // Use the primary constructor directly
    val primaryConstructor = typeSymbol.primaryConstructor

    // Check if we need to handle type parameters
    val constructorCall = tpe match {
      case AppliedType(_, typeArgs) =>
        New(TypeTree.of[T]).select(primaryConstructor).appliedToTypes(typeArgs).appliedToArgs(fieldExprs.map(_.asTerm))
      case _ =>
        // For non-generic types
        New(TypeTree.of[T]).select(primaryConstructor).appliedToArgs(fieldExprs.map(_.asTerm))
    }

    constructorCall.asExprOf[T]
  }
}