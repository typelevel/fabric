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
import fabric.define.{DefType, Definition as FabricDefinition, Format, GenericType as FabricGenericType}

import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*

import scala.util.Try

import scala.collection.immutable.VectorMap

private def safeSimpleName(cls: Class[_]): String = {
  val name = cls.getName
  val lastDot = name.lastIndexOf('.')
  val lastDollar = name.lastIndexOf('$')
  val start = math.max(lastDot, lastDollar) + 1
  name.substring(start)
}

private def safeTypeName(value: Any): String = value match {
  case p: Product => p.productPrefix
  case _ => safeTypeName(value)
}

@nowarn()
trait CompileRW {
  inline final def derived[T](using ct: ClassTag[T]): RW[T] = gen[T]

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

    override def definition: FabricDefinition = FabricDefinition(DefType.Str)
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
      val typeName = safeTypeName(value)
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

    override def definition: FabricDefinition = {
      val childDefs = childRWs.map { case (name, rw) =>
        name -> rw.definition
      }.toMap.to(VectorMap)
      FabricDefinition(DefType.Poly(childDefs), className = Some(getSimpleTypeName[T]))
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

  inline def getFullTypeName[T]: String = ${ CompileRW.getFullTypeNameImpl[T] }

  inline def findValueCaseInsensitive(map: Map[String, Json], key: String): Option[Json] =
    map.get(key).orElse(map.collectFirst { case (k, v) if k.equalsIgnoreCase(key) => v })

  // Helper for case objects
  inline def singleton[T](instance: T): RW[T] = new RW[T] {
    override def read(value: T): Json = Obj()
    override def write(json: Json): T = instance
    override def definition: FabricDefinition = FabricDefinition(DefType.Obj(Map.empty), className = Some(getFullTypeName[T]))
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

    override def definition: FabricDefinition = FabricDefinition(
      DefType.Poly(instanceToName.values.toList.map(n => n -> FabricDefinition(DefType.Null)).toMap.to(VectorMap)),
      className = Some(getFullTypeName[T])
    )
  }

  inline def enumName[T](value: T)(using m: Mirror.SumOf[T]): String =
    value.asInstanceOf[scala.reflect.Enum].productPrefix

  inline def fromName[T](name: String)(using m: Mirror.SumOf[T]): T =
    ${ CompileRW.fromNameImpl[T]('name) }

  inline def toClassName[T](using ct: ClassTag[T]): Option[String] =
    Some(ct.runtimeClass.getName.replace("$", "."))

  inline def toDefinition[T](using p: Mirror.ProductOf[T], ct: ClassTag[T]): FabricDefinition = {
    FabricDefinition(DefType.Obj(toDefinitionElems[T, p.MirroredElemTypes, p.MirroredElemLabels](0)), className = toClassName[T])
  }

  inline def toDefinitionElems[A, T <: Tuple, L <: Tuple](index: Int): Map[String, FabricDefinition] = {
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
            val hdValueOption: Option[Json] = findValueCaseInsensitive(map, hdLabelValue)
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
              case json => RWFieldHelper.writeField(hdWritable, json, getClassName[A], hdLabelValue)
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
  def applyFieldDescriptions(d: FabricDefinition, descs: Map[String, String]): FabricDefinition = {
    if (descs.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
        descs.get(k).fold(k -> v)(desc => k -> v.describe(desc))
      }))
      case _ => d
    }
  }

  def removeTransientFields(d: FabricDefinition, fields: Set[String]): FabricDefinition = {
    if (fields.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map -- fields))
      case _ => d
    }
  }

  def applySerializedFields(d: FabricDefinition, extraFields: Map[String, FabricDefinition]): FabricDefinition = {
    if (extraFields.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map ++ extraFields))
      case _ => d
    }
  }

  /** Write a field value with error context wrapping. Non-inline to ensure try-catch works. */
  def writeField[T](writer: Writer[T], json: Json, className: String, fieldName: String): T = {
    try {
      writer.write(json)
    } catch {
      case e: RWException => throw e.withPath(s"$className.$fieldName")
      case e: Exception => throw RWException(
        s"Failed to deserialize field '$fieldName': ${e.getMessage}",
        path = List(s"$className.$fieldName")
      )
    }
  }

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

    val fullName = TypeTree.of[T].symbol.companionClass.fullName.replace("$", ".")
    Expr(fullName)
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

  def getFullTypeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    // Get the full qualified name
    Expr(symbol.fullName)
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

    // Check if it's a union type (A | B | C)
    tpe.dealias match {
      case or: OrType =>
        genUnionMacro[T](or)
      case _ =>
        // Check simple enums FIRST — simple enums (all case objects) have valueOf and should use the enum path.
        // Parameterized enums (with case class children) don't have valueOf and fall through to sealed trait.
        if (typeSymbol.flags.is(Flags.Enum) && typeSymbol.companionModule.methodMember("valueOf").nonEmpty) {
          genEnumMacro[T]()
        } else if (typeSymbol.flags.is(Flags.Sealed) && (typeSymbol.flags.is(Flags.Trait) || typeSymbol.flags.is(Flags.Abstract) || typeSymbol.flags.is(Flags.Enum))) {
          // Get child types directly from symbol — avoids Mirror.SumOf which has compatibility issues
          val children = typeSymbol.children
          val allCaseObjects = children.forall { child =>
            child.flags.is(Flags.Module) && child.flags.is(Flags.Case)
          }

          if (allCaseObjects) {
            val instances = children.map { child =>
              Ref(child.termRef.termSymbol).asExprOf[T]
            }
            val instancesList = Expr.ofList(instances)
            '{ RW.enumeration[T]($instancesList) }
          } else {
            genSealedTraitFromChildren[T](children)
          }
        } else if (typeSymbol.flags.is(Flags.Case) && typeSymbol.isClassDef && tpe <:< TypeRepr.of[AnyVal]) {
          // Handle AnyVal case classes — proxy to the inner type's RW
          genAnyValMacro[T]
        } else if (typeSymbol.flags.is(Flags.Case) && typeSymbol.isClassDef) {
          // Handle case classes - call genMacro directly
          genMacro[T]
        } else {
          report.errorAndAbort(s"${typeSymbol.name} is not a supported type for RW generation. It must be a case class, enum, sealed trait, or union type.")
        }
    }
  }

  def genEnumMacro[T: Type]()(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    // Generate valueOf lookup at macro time
    val valueOfExpr = generateValueOfLookup[T]()

    // Extract enum case names for DefType.Poly
    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    val caseNames = typeSymbol.children.filter(c => c.flags.is(Flags.Case)).map(_.name.stripSuffix("$"))
    val caseNamesExpr = Expr(caseNames)
    val classNameExpr = Expr(typeSymbol.fullName.replace("$", "."))

    '{
      new RW[T] {
        override def read(value: T): Json = Str(value.asInstanceOf[scala.reflect.Enum].productPrefix)

        override def write(json: Json): T = json match {
          case Str(name, _) => $valueOfExpr(name)
          case _ => throw RWException(s"Expected string for enum, got: $json")
        }

        override val definition: FabricDefinition = FabricDefinition(
          DefType.Poly($caseNamesExpr.map(n => n -> FabricDefinition(DefType.Null)).toMap.to(VectorMap)),
          className = Some($classNameExpr)
        )
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

  def genSealedTraitFromChildren[T: Type](children: List[Any])(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val childSymbols = children.asInstanceOf[List[Symbol]]
    val childTypes = childSymbols.map { child =>
      if (child.flags.is(Flags.Module)) child.termRef
      else child.typeRef
    }

    val childExprs = childSymbols.zip(childTypes).map { case (childSym, childType) =>
      childType.asType match {
        case '[t] =>
          val rw = Expr.summon[RW[t]].getOrElse {
            // No existing RW — generate one for case class or case object children
            if (childSym.isClassDef && childSym.flags.is(Flags.Case)) {
              genMacro[t]
            } else if (childSym.flags.is(Flags.Module)) {
              val ref = Ref(childSym.termRef.termSymbol).asExprOf[t]
              '{ RW.static[t]($ref) }
            } else if (childSym.flags.is(Flags.Enum) && childSym.flags.is(Flags.Case) && !childSym.isClassDef) {
              // Simple enum case (e.g., `case Point` in a mixed enum) — treated as singleton
              val ref = Ref(childSym.termRef.termSymbol).asExprOf[t]
              '{ RW.static[t]($ref) }
            } else {
              report.errorAndAbort(s"No RW found for child type ${childType.show}. Provide an RW instance or make it a case class.")
            }
          }
          val name = getSimpleTypeNameFromType(childType)
          '{ (${ Expr(name) }, $rw.asInstanceOf[RW[_]]) }
      }
    }
    val childRWsExpr = Expr.ofList(childExprs)
    genPolyRW[T](childRWsExpr)
  }

  /** Shared polymorphic RW generation for sealed traits and union types.
    * Uses "type" discriminator field by default (consistent with RW.poly),
    * configurable via @typeField("customName") annotation on the type. */
  private def genPolyRW[T: Type](childRWsExpr: Expr[List[(String, RW[_])]])(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Check for @typeField annotation to customize the discriminator field name
    val fieldName = typeSymbol.annotations.collectFirst {
      case ann if ann.tpe.typeSymbol.fullName == "fabric.rw.typeField" =>
        ann match {
          case Apply(_, List(Literal(StringConstant(name)))) => name
          case _ => "type"
        }
    }.getOrElse("type")
    val fieldNameExpr = Expr(fieldName)

    val fullTypeName = typeSymbol.fullName.replace("$", ".")
    val fullTypeNameExpr = Expr(fullTypeName)

    '{
      new RW[T] {
        private val typeField = $fieldNameExpr
        private lazy val childRWs = $childRWsExpr

        override def read(value: T): Json = {
          val typeName = safeTypeName(value)
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
                throw RWException(s"Missing or invalid '$typeField' field in JSON for polymorphic type")
            }
          case _ =>
            throw RWException(s"Expected JSON object for polymorphic type, got: $json")
        }

        override def definition: FabricDefinition = {
          val childDefs = childRWs.map { case (name, rw) =>
            name -> rw.definition
          }.toMap
          FabricDefinition(DefType.Poly(childDefs), className = Some($fullTypeNameExpr))
        }
      }
    }
  }

  /** Generate RW for Scala 3 union types (A | B | C).
    * Handles the case where multiple union members share the same base class but differ by type parameters
    * (e.g. `Id[String] | Id[Int]`) by using full parameterized type names as discriminators. */
  def genUnionMacro[T: Type](orType: Any)(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    // Flatten A | B | C into List(TypeRepr)
    def flattenUnion(tpe: TypeRepr): List[TypeRepr] = tpe match {
      case or: OrType => flattenUnion(or.left) ++ flattenUnion(or.right)
      case other => List(other)
    }

    val memberTypes = flattenUnion(orType.asInstanceOf[TypeRepr])

    // Detect if any members share the same base class (type parameter collision)
    val simpleNames = memberTypes.map(t => getSimpleTypeNameFromType(t))
    val hasCollisions = simpleNames.distinct.size != simpleNames.size

    if (hasCollisions) {
      genCollisionUnionMacro[T](memberTypes)
    } else {
      // No collisions — use standard poly generation
      val childExprs = memberTypes.map { memberType =>
        memberType.asType match {
          case '[t] =>
            val rw = Expr.summon[RW[t]].getOrElse {
              val childSym = memberType.typeSymbol
              if (childSym.isClassDef && childSym.flags.is(Flags.Case)) {
                genMacro[t]
              } else {
                report.errorAndAbort(s"No RW found for union member type ${memberType.show}. Ensure all union member types have an RW instance.")
              }
            }
            val name = getSimpleTypeNameFromType(memberType)
            '{ (${ Expr(name) }, $rw.asInstanceOf[RW[_]]) }
        }
      }
      val childRWsExpr = Expr.ofList(childExprs)
      genPolyRW[T](childRWsExpr)
    }
  }

  /** Generate RW for union types where multiple members share the same base class (e.g. `Id[String] | Id[Int]`).
    * Uses full parameterized type names as discriminators and compile-time type matching for the write path
    * since runtime class inspection can't distinguish erased generic variants. */
  private def genCollisionUnionMacro[T: Type](memberTypes: List[Any])(using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val members = memberTypes.asInstanceOf[List[TypeRepr]]

    // Build child RW pairs — always generate fresh RWs (not summoned) so each gets concrete _generic info
    val childExprs = members.map { memberType =>
      memberType.asType match {
        case '[t] =>
          val childSym = memberType.typeSymbol
          // Always generate fresh to ensure _generic reflects the concrete type args
          val rw = if (childSym.isClassDef && childSym.flags.is(Flags.Case)) {
            genMacro[t]
          } else {
            Expr.summon[RW[t]].getOrElse {
              report.errorAndAbort(s"No RW found for union member type ${memberType.show}.")
            }
          }
          val simpleName = getSimpleTypeNameFromType(memberType)
          val fullName = fullTypeName(memberType)
          '{ (${ Expr(simpleName) }, ${ Expr(fullName) }, $rw.asInstanceOf[RW[_]]) }
      }
    }
    val childListExpr = Expr.ofList(childExprs)

    val fullTypeNameStr = members.map(fullTypeName(_)).mkString(" | ")
    val fullTypeNameExpr = Expr(fullTypeNameStr)

    '{
      new RW[T] {
        private val typeField = "type"
        private lazy val childRWs: List[(String, String, RW[_])] = $childListExpr

        private def matchGeneric(json: Json, candidates: List[(String, String, RW[_])]): Option[(String, RW[_])] = {
          json match {
            case Obj(map) =>
              map.get("_generic") match {
                case Some(genericJson) =>
                  // Match by comparing _generic content against each candidate's definition.genericTypes
                  candidates.find { case (_, _, rw) =>
                    val expected = Obj(rw.definition.genericTypes.map(gt => gt.name -> gt.definition.json): _*)
                    expected == genericJson
                  }.map(c => (c._2, c._3))
                case None =>
                  // No _generic field — take first candidate
                  candidates.headOption.map(c => (c._2, c._3))
              }
            case _ => candidates.headOption.map(c => (c._2, c._3))
          }
        }

        override def read(value: T): Json = {
          val simpleName = safeTypeName(value)
          val candidates = childRWs.filter(_._1 == simpleName)
          // Use first candidate for read — the child RW will embed _generic in its output
          candidates.headOption match {
            case Some((_, _, rw)) =>
              rw.asInstanceOf[RW[T]].read(value) match {
                case obj: Obj => obj.merge(Obj(typeField -> Str(simpleName)))
                case other => Obj(typeField -> Str(simpleName), "value" -> other)
              }
            case None => throw RWException(s"Unknown subtype: $simpleName")
          }
        }

        override def write(json: Json): T = json match {
          case obj @ Obj(map) =>
            map.get(typeField) match {
              case Some(Str(typeName, _)) =>
                val candidates = childRWs.filter(_._1 == typeName)
                val (_, rw) = if (candidates.size > 1) {
                  // Collision — use _generic to disambiguate
                  val cleanedJson = Obj(map - typeField)
                  matchGeneric(cleanedJson, candidates).getOrElse(
                    throw RWException(s"Cannot disambiguate type '$typeName' — no matching _generic found. Available: ${candidates.map(_._2).mkString(", ")}")
                  )
                } else {
                  candidates.headOption.map(c => (c._2, c._3)).getOrElse(
                    throw RWException(s"Unknown type discriminator: $typeName")
                  )
                }
                val cleanedMap = map - typeField
                val cleanedJson = if (cleanedMap.isEmpty && map.size == 2 && map.contains("value")) {
                  map("value")
                } else {
                  Obj(cleanedMap)
                }
                rw.asInstanceOf[RW[T]].write(cleanedJson)
              case _ =>
                throw RWException(s"Missing or invalid '$typeField' field in JSON for union type")
            }
          case _ =>
            throw RWException(s"Expected JSON object for union type, got: $json")
        }

        override def definition: FabricDefinition = {
          val childDefs = childRWs.map { case (_, fullName, rw) =>
            fullName -> rw.definition
          }.toMap.to(VectorMap)
          FabricDefinition(DefType.Poly(childDefs), className = Some($fullTypeNameExpr))
        }
      }
    }
  }

  private def cleanFullName(name: String): String =
    name.replace("$.", ".").replace("$", ".")

  private def fullTypeName(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect._
    val base = cleanFullName(tpe.typeSymbol.fullName)
    tpe match {
      case AppliedType(_, args) =>
        s"$base[${args.map(fullTypeName(_)).mkString(", ")}]"
      case _ => base
    }
  }

  private def generateGenericTypes(using Quotes)(tpe: quotes.reflect.TypeRepr): Expr[List[FabricGenericType]] = {
    import quotes.reflect._
    val typeSymbol = tpe.typeSymbol
    val typeParamNames = typeSymbol.primaryConstructor.paramSymss.headOption match {
      case Some(params) if params.nonEmpty && params.head.isTypeParam => params.map(_.name)
      case _ => Nil
    }
    tpe match {
      case AppliedType(_, args) if typeParamNames.nonEmpty =>
        val entries = typeParamNames.zip(args).flatMap { case (name, arg) =>
          val nameExpr = Expr(name)
          arg.asType match {
            case '[t] =>
              Expr.summon[RW[t]].map { rw =>
                '{ FabricGenericType($nameExpr, $rw.definition) }
              }
          }
        }
        Expr.ofList(entries)
      case _ =>
        '{ Nil }
    }
  }

  private def extractFieldGenericNames(using Quotes)(tpe: quotes.reflect.TypeRepr): Expr[Map[String, String]] = {
    val typeSymbol = tpe.typeSymbol
    val typeParamSymbols = typeSymbol.primaryConstructor.paramSymss.headOption match {
      case Some(params) if params.nonEmpty && params.head.isTypeParam => params
      case _ => Nil
    }
    if (typeParamSymbols.isEmpty) {
      '{ Map.empty }
    } else {
      val typeParamNames = typeParamSymbols.map(_.name).toSet
      val fields = typeSymbol.caseFields
      val entries = fields.flatMap { field =>
        val fieldType = tpe.memberType(field)
        findTypeParamName(fieldType, typeParamNames).map { paramName =>
          val fieldName = Expr(field.name)
          val paramNameExpr = Expr(paramName)
          '{ ($fieldName, $paramNameExpr) }
        }
      }
      val entriesList = Expr.ofList(entries)
      '{ $entriesList.toMap }
    }
  }

  private def findTypeParamName(using Quotes)(tpe: quotes.reflect.TypeRepr, typeParamNames: Set[String]): Option[String] = {
    import quotes.reflect._
    val name = tpe.typeSymbol.name
    if (typeParamNames.contains(name)) Some(name)
    else tpe match {
      case AppliedType(_, args) => args.flatMap(findTypeParamName(_, typeParamNames)).headOption
      case _ => None
    }
  }

  private def getSimpleTypeNameFromType(tpe: Any)(using Quotes): String = {
    import quotes.reflect._
    val typeRepr = tpe.asInstanceOf[TypeRepr]
    val symbol = typeRepr.typeSymbol
    val fullName = symbol.fullName
    val simpleName = fullName.split('.').last
    simpleName.stripSuffix("$")
  }

  def genAnyValMacro[T: Type](using Quotes): Expr[RW[T]] = {
    import quotes.reflect._

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    val field = typeSymbol.primaryConstructor.paramSymss.flatten.head
    val fieldType = tpe.memberType(field)

    fieldType.asType match {
      case '[inner] =>
        val innerRW = Expr.summon[RW[inner]].getOrElse {
          report.errorAndAbort(s"No RW found for AnyVal inner type ${fieldType.show}")
        }
        val classNameExpr = Expr(cleanFullName(typeSymbol.fullName))
        val genericTypesExpr = generateGenericTypes(tpe)
        '{
          new RW[T] {
            override def read(value: T): Json = {
              val inner = value.asInstanceOf[Product].productElement(0).asInstanceOf[inner]
              $innerRW.read(inner)
            }
            override def write(json: Json): T = {
              val inner = $innerRW.write(json)
              ${ Apply(Select(New(TypeTree.of[T]), typeSymbol.primaryConstructor), List('{ inner }.asTerm)).asExprOf[T] }
            }
            override val definition: FabricDefinition = $innerRW.definition.withClassName($classNameExpr).copy(genericTypes = $genericTypesExpr)
          }
        }
    }
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

    // Extract @description annotations from constructor parameters
    val fieldDescs = extractFieldDescriptions(typeSymbol)
    val fieldDescsExpr = Expr(fieldDescs)

    // Extract @serialized members (vals and no-arg defs)
    val serializedMembers = extractSerializedMembers[T](typeSymbol)

    // Extract @transient constructor params
    val transientFields = extractTransientFields(typeSymbol)
    val transientFieldsExpr = Expr(transientFields)

    val hasExtra = serializedMembers.nonEmpty
    val hasTransient = transientFields.nonEmpty

    val extraMapExpr = if (hasExtra) Some(generateSerializedFieldsMap[T](serializedMembers)) else None
    val extraDefExpr = if (hasExtra) Some(generateSerializedFieldsDef(serializedMembers)) else None

    // Extract @format annotations
    val fieldFormats = extractFieldFormats(typeSymbol)
    val fieldFormatsExpr = if (fieldFormats.isEmpty) '{ Map.empty[String, Format] }
    else {
      val entries = fieldFormats.map { case (k, v) =>
        val keyExpr = Expr(k)
        val valExpr = Expr(v)
        '{ ($keyExpr, Format.values.find(_.name == $valExpr).getOrElse(Format.Raw)) }
      }.toList
      val list = Expr.ofList(entries)
      '{ $list.toMap }
    }

    // Extract @fieldDeprecated annotations
    val deprecatedFields = extractDeprecatedFields(typeSymbol)
    val deprecatedFieldsExpr = Expr(deprecatedFields)

    // Extract default values
    val fieldDefaultsExpr = generateFieldDefaults[T](typeSymbol)

    val classNameExpr = Expr(fullTypeName(tpe))
    val genericTypesExpr = generateGenericTypes(tpe)
    val fieldGenericNamesExpr = extractFieldGenericNames(tpe)

    // Check if this type has type arguments (is a generic instantiation)
    val typeParamSyms = typeSymbol.primaryConstructor.paramSymss.headOption match {
      case Some(params) if params.nonEmpty && params.head.isTypeParam => params
      case _ => Nil
    }
    val hasTypeArgs = tpe match {
      case AppliedType(_, _) if typeParamSyms.nonEmpty => true
      case _ => false
    }

    // Generate _generic JSON value at macro time
    val genericJsonExpr: Option[Expr[Json]] = if (hasTypeArgs) {
      tpe match {
        case AppliedType(_, args) =>
          val entries = typeParamSyms.zip(args).map { case (param, arg) =>
            val nameExpr = Expr(param.name)
            arg.asType match {
              case '[t] =>
                val rw = Expr.summon[RW[t]].getOrElse {
                  report.errorAndAbort(s"No RW found for type parameter ${param.name} (${arg.show})")
                }
                '{ ($nameExpr, $rw.definition.json) }
            }
          }
          val list = Expr.ofList(entries)
          Some('{ Obj($list: _*) })
        case _ => None
      }
    } else None

    '{
      new ClassRW[T] {
        override protected def t2Map(t: T): Map[String, Json] = {
          val base = CompileRW.toMap(t)(using $mirror)
          val withExtra = ${ extraMapExpr match {
            case Some(gen) => '{ base ++ ${ gen('{t}) } }
            case None => '{ base }
          }}
          val withTransient = ${ if (hasTransient) '{ withExtra -- $transientFieldsExpr }
             else '{ withExtra } }
          ${ genericJsonExpr match {
            case Some(gj) => '{ if (RW.SerializeGenerics) withTransient + ("_generic" -> $gj) else withTransient }
            case None => '{ withTransient }
          }}
        }

        override protected def map2T(map: Map[String, Json]): T = {
          ${ generateDirectConstructor[T]('{map}) }
        }

        override def definition: FabricDefinition = {
          val baseDef = FabricDefinition.applyFieldDefaults(
            FabricDefinition.applyFieldDeprecations(
              FabricDefinition.applyFieldFormats(
                FabricDefinition.applyGenericNames(
                  CompileRW.applyFieldDescriptions(
                    CompileRW.toDefinition[T](using $mirror, $ct),
                    $fieldDescsExpr
                  ),
                  $fieldGenericNamesExpr
                ),
                $fieldFormatsExpr
              ),
              $deprecatedFieldsExpr
            ),
            $fieldDefaultsExpr
          ).withClassName($classNameExpr).copy(genericTypes = $genericTypesExpr)
          ${ (hasExtra, hasTransient) match {
            case (true, true) =>
              '{ CompileRW.removeTransientFields(CompileRW.applySerializedFields(baseDef, ${ extraDefExpr.get }), $transientFieldsExpr) }
            case (true, false) =>
              '{ CompileRW.applySerializedFields(baseDef, ${ extraDefExpr.get }) }
            case (false, true) =>
              '{ CompileRW.removeTransientFields(baseDef, $transientFieldsExpr) }
            case (false, false) =>
              '{ baseDef }
          }}
        }
      }
    }
  }

  private def extractFieldDescriptions(typeSymbol: Any)(using Quotes): Map[String, String] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    sym.primaryConstructor.paramSymss.flatten.flatMap { param =>
      param.annotations.collectFirst {
        case ann if ann.tpe.typeSymbol.fullName == "fabric.rw.description" =>
          ann match {
            case Apply(_, List(Literal(StringConstant(text)))) => param.name -> text
          }
      }
    }.toMap
  }

  private def extractTransientFields(typeSymbol: Any)(using Quotes): Set[String] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    sym.primaryConstructor.paramSymss.flatten.flatMap { param =>
      val isTransient = param.annotations.exists { ann =>
        ann.tpe.typeSymbol.fullName == "fabric.rw.notSerialized"
      }
      if (isTransient) Some(param.name) else None
    }.toSet
  }

  private def extractFieldFormats(typeSymbol: Any)(using Quotes): Map[String, String] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    sym.primaryConstructor.paramSymss.flatten.flatMap { param =>
      param.annotations.collectFirst {
        case ann if ann.tpe.typeSymbol.fullName == "fabric.rw.format" =>
          ann match {
            case Apply(_, List(Select(_, name))) => param.name -> name.toLowerCase
            case Apply(_, List(arg)) =>
              // Try to extract the Format object's name field
              arg.tpe.termSymbol.name.stripSuffix("$").toLowerCase match {
                case name => param.name -> name
              }
          }
      }
    }.toMap
  }

  private def extractDeprecatedFields(typeSymbol: Any)(using Quotes): Set[String] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    sym.primaryConstructor.paramSymss.flatten.flatMap { param =>
      val isDeprecated = param.annotations.exists { ann =>
        ann.tpe.typeSymbol.fullName == "fabric.rw.fieldDeprecated"
      }
      if (isDeprecated) Some(param.name) else None
    }.toSet
  }

  private def generateFieldDefaults[T: Type](typeSymbol: Any)(using Quotes): Expr[Map[String, Json]] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    val comp = sym.companionClass
    val body = comp.tree.asInstanceOf[ClassDef].body

    val defaultDefs = (for {
      case deff @ DefDef(name, _, _, _) <- body
      if name.startsWith("$lessinit$greater$default")
    } yield deff).toList

    if (defaultDefs.isEmpty) '{ Map.empty }
    else {
      val fields = sym.caseFields
      val entries = defaultDefs.flatMap { deff =>
        val index = deff.name.stripPrefix("$lessinit$greater$default$").toInt - 1
        if (index < fields.length) {
          val fieldName = Expr(fields(index).name)
          val fieldType = TypeRepr.of[T].memberType(fields(index))
          fieldType.asType match {
            case '[ft] =>
              val reader = Expr.summon[Reader[ft]]
              reader.map { r =>
                val ref = Ref(deff.symbol).asExprOf[ft]
                '{ ($fieldName, $r.read($ref)) }
              }
          }
        } else None
      }
      if (entries.isEmpty) '{ Map.empty }
      else {
        val list = Expr.ofList(entries)
        '{ $list.toMap }
      }
    }
  }

  private case class SerializedMember(jsonKey: String, memberName: String, memberType: Any)

  private def extractSerializedMembers[T: Type](typeSymbol: Any)(using Quotes): List[SerializedMember] = {
    import quotes.reflect._
    val sym = typeSymbol.asInstanceOf[Symbol]
    sym.declarations.flatMap { member =>
      member.annotations.collectFirst {
        case ann if ann.tpe.typeSymbol.fullName == "fabric.rw.serialized" =>
          val customName = ann match {
            case Apply(_, List(Literal(StringConstant(name)))) if name.nonEmpty => name
            case _ => member.name
          }
          val returnType = if (member.isDefDef) {
            val defDef = member.tree.asInstanceOf[DefDef]
            defDef.returnTpt.tpe
          } else if (member.isValDef) {
            val valDef = member.tree.asInstanceOf[ValDef]
            valDef.tpt.tpe
          } else {
            report.errorAndAbort(s"@serialized can only be applied to vals or no-arg defs, but ${member.name} is neither")
          }
          SerializedMember(customName, member.name, returnType)
      }
    }.toList
  }

  private def generateSerializedFieldsMap[T: Type](members: List[SerializedMember])(using Quotes): Expr[T] => Expr[Map[String, Json]] = {
    import quotes.reflect._
    (tExpr: Expr[T]) => {
      val entries = members.map { m =>
        val memberType = m.memberType.asInstanceOf[TypeRepr]
        memberType.asType match {
          case '[mt] =>
            val reader = Expr.summon[Reader[mt]].getOrElse {
              report.errorAndAbort(s"No Reader found for @serialized member '${m.memberName}' of type ${memberType.show}")
            }
            val keyExpr = Expr(m.jsonKey)
            val valueExpr = Select(tExpr.asTerm, TypeRepr.of[T].typeSymbol.methodMember(m.memberName).head).asExprOf[mt]
            '{ ($keyExpr, $reader.read($valueExpr)) }
        }
      }
      val entriesList = Expr.ofList(entries)
      '{ $entriesList.toMap }
    }
  }

  private def generateSerializedFieldsDef(members: List[SerializedMember])(using Quotes): Expr[Map[String, FabricDefinition]] = {
    import quotes.reflect._
    val entries = members.map { m =>
      val memberType = m.memberType.asInstanceOf[TypeRepr]
      memberType.asType match {
        case '[mt] =>
          val rw = Expr.summon[RW[mt]].getOrElse {
            report.errorAndAbort(s"No RW found for @serialized member '${m.memberName}' of type ${memberType.show}")
          }
          val keyExpr = Expr(m.jsonKey)
          '{ ($keyExpr, $rw.definition) }
      }
    }
    val entriesList = Expr.ofList(entries)
    '{ $entriesList.toMap }
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
          val classNameExpr = Expr(typeSymbol.fullName.replace("$", "."))

          '{
            val defaults = $defaultsExpr
            val jsonOpt = _root_.fabric.rw.CompileRW.findValueCaseInsensitive($map, $fieldNameStr)
            val defaultOpt = defaults.get($fieldNameStr)

            jsonOpt match {
              case Some(json) =>
                json match {
                  case fabric.Null if defaultOpt.isDefined => defaultOpt.get.asInstanceOf[ft]
                  case _ => _root_.fabric.rw.RWFieldHelper.writeField($writer, json, $classNameExpr, $fieldNameStr)
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