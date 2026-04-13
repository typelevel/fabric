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

import fabric.JsonWrapper
import fabric.define.Definition

import scala.reflect.macros.blackbox

object RWMacros {
  private def fullTypeName(context: blackbox.Context)(tpe: context.universe.Type): String = {
    val base = tpe.typeSymbol.fullName
    val args = tpe.typeArgs
    if (args.isEmpty) base
    else s"$base[${args.map(fullTypeName(context)(_)).mkString(", ")}]"
  }

  private def generateGenericTypes(context: blackbox.Context)(tpe: context.universe.Type): List[context.universe.Tree] = {
    import context.universe._
    val typeArgs = tpe.typeArgs
    val typeParams = tpe.typeSymbol.asClass.typeParams
    if (typeArgs.isEmpty) Nil
    else typeParams.zip(typeArgs).map { case (param, arg) =>
      val name = param.name.decodedName.toString
      q"GenericType($name, implicitly[RW[$arg]].definition)"
    }
  }

  private def extractFieldGenericNames(context: blackbox.Context)(tpe: context.universe.Type): Map[String, String] = {
    import context.universe._
    val typeParams = tpe.typeSymbol.asClass.typeParams.map(_.name.decodedName.toString).toSet
    if (typeParams.isEmpty) Map.empty
    else {
      tpe.decls
        .collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
        }
        .getOrElse(Nil)
        .flatMap { field =>
          val fieldName = field.asTerm.name.decodedName.toString
          val fieldType = field.typeSignature
          findTypeParamName(context)(fieldType, typeParams).map(fieldName -> _)
        }
        .toMap
    }
  }

  private def findTypeParamName(
    context: blackbox.Context
  )(tpe: context.universe.Type, typeParamNames: Set[String]): Option[String] = {
    val name = tpe.typeSymbol.name.decodedName.toString
    if (typeParamNames.contains(name)) Some(name)
    else tpe.typeArgs.flatMap(findTypeParamName(context)(_, typeParamNames)).headOption
  }

  private def extractFieldFormats(
    context: blackbox.Context
  )(fields: List[context.universe.Symbol]): Map[String, context.universe.Tree] = {
    import context.universe._
    fields.flatMap { field =>
      val key = field.asTerm.name.decodedName.toString
      field.annotations
        .collectFirst {
          case ann if ann.tree.tpe =:= typeOf[format] =>
            ann.tree.children.tail.headOption match {
              case Some(arg) => Some(key -> arg)
              case None => None
            }
        }
        .getOrElse(None)
    }.toMap
  }

  private def extractDeprecatedFields(context: blackbox.Context)(fields: List[context.universe.Symbol]): Set[String] = {
    import context.universe._
    fields.flatMap { field =>
      val isDeprecated = field.annotations.exists(_.tree.tpe =:= typeOf[fieldDeprecated])
      if (isDeprecated) Some(field.asTerm.name.decodedName.toString) else None
    }.toSet
  }

  private def generateFieldDefaults(context: blackbox.Context)(
    fields: List[context.universe.Symbol],
    defaults: Map[Int, context.universe.MethodSymbol],
    companion: context.universe.Symbol,
    tpe: context.universe.Type
  ): List[context.universe.Tree] = {
    import context.universe._
    fields.zipWithIndex.flatMap { case (field, index) =>
      defaults.get(index).map { m =>
        val key = field.asTerm.name.decodedName.toString
        val returnType = tpe.decl(field.asTerm.name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
        q"$key -> implicitly[Reader[$returnType]].read($companion.$m)"
      }
    }
  }

  def caseClassD[T](
    context: blackbox.Context
  )(implicit t: context.WeakTypeTag[T]): context.Expr[Definition] = {
    import context.universe._

    val tpe = t.tpe
    val className = fullTypeName(context)(tpe)
    val companion: Symbol = tpe.typeSymbol.companion
    val defaults = defaultsFor(context)(companion)
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) =>
        val transientNames = fields
          .filter(_.annotations.exists(_.tree.tpe =:= typeOf[notSerialized]))
          .map(_.asTerm.name.decodedName.toString)
          .toSet
        val fieldDefs = fields.zipWithIndex
          .filterNot { case (field, _) => transientNames.contains(field.asTerm.name.decodedName.toString) }
          .map { case (field, index) =>
            val name = field.asTerm.name
            val key = name.decodedName.toString
            val returnType = tpe.decl(name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
            val descAnn = field.annotations.find(_.tree.tpe =:= typeOf[description])
            val baseDef =
              if (defaults.contains(index)) {
                q"implicitly[RW[$returnType]].definition.opt"
              } else {
                q"implicitly[RW[$returnType]].definition"
              }
            descAnn match {
              case Some(ann) => ann.tree.children.tail.head match {
                  case l: LiteralApi =>
                    val text = l.value.value.toString
                    q"$key -> $baseDef.describe($text)"
                  case _ => q"$key -> $baseDef"
                }
              case None => q"$key -> $baseDef"
            }
          }
        val serializedDefs = serializedMembers(context)(tpe).map { info =>
          val key = info.jsonKey
          val returnType = info.returnType.asInstanceOf[Type]
          q"$key -> implicitly[RW[$returnType]].definition"
        }
        val allFieldDefs = fieldDefs ++ serializedDefs
        val genericTypes = generateGenericTypes(context)(tpe)
        val fieldGenericNames = extractFieldGenericNames(context)(tpe)
        val fieldGenericNamesEntries = fieldGenericNames.map { case (k, v) => q"$k -> $v" }.toList
        val fieldFormats = extractFieldFormats(context)(fields)
        val fieldFormatsEntries = fieldFormats.map { case (k, v) => q"$k -> $v" }.toList
        val deprecatedFields = extractDeprecatedFields(context)(fields)
        val deprecatedFieldsList = deprecatedFields.toList
        val fieldDefaultEntries = generateFieldDefaults(context)(fields, defaults, companion, tpe)
        context.Expr[Definition](q"""
            import _root_.fabric._
            import _root_.fabric.define._
            import _root_.scala.collection.immutable.VectorMap

            Definition.applyFieldDefaults(
              Definition.applyFieldDeprecations(
                Definition.applyFieldFormats(
                  Definition.applyGenericNames(
                    Definition(DefType.Obj(VectorMap(..$allFieldDefs)), className = Some($className), genericTypes = List(..$genericTypes)),
                    Map(..$fieldGenericNamesEntries)
                  ),
                  Map(..$fieldFormatsEntries)
                ),
                Set(..$deprecatedFieldsList)
              ),
              Map(..$fieldDefaultEntries)
            )
           """)
      case None =>
        val caseObjects = companion.typeSignature.members.collect {
          case s: ModuleSymbol if s.moduleClass.asType.toType <:< tpe => s.name
        }.toList

        if (caseObjects.isEmpty) {
          context.abort(context.enclosingPosition, "Not a valid case class or sealed trait with case objects")
        } else {
          context.Expr[Definition](q"""
            import _root_.fabric._
            import _root_.fabric.define._
            import _root_.fabric.rw._

            RW.enumeration[$tpe](List(..$caseObjects)).definition
           """)
        }
    }
  }

  def caseClassR[T](
    context: blackbox.Context
  )(implicit t: context.WeakTypeTag[T]): context.Expr[Reader[T]] = {
    import context.universe._

    val tpe = t.tpe
    val companion: Symbol = tpe.typeSymbol.companion
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) =>
        val transientNames = fields
          .filter(_.annotations.exists(_.tree.tpe =:= typeOf[notSerialized]))
          .map(_.asTerm.name.decodedName.toString)
          .toSet
        val toMap: List[context.universe.Tree] = fields
          .filterNot { field =>
            transientNames.contains(field.asTerm.name.decodedName.toString)
          }
          .map { field =>
            val name = field.asTerm.name
            val key = name.decodedName.toString
            q"$key -> t.$name.json"
          }
        val extraMap: List[context.universe.Tree] = serializedMembers(context)(tpe).map { info =>
          val key = info.jsonKey
          val memberName = TermName(info.memberName)
          q"$key -> t.$memberName.json"
        }
        val allMap = toMap ++ extraMap
        context.Expr[Reader[T]](q"""
            import _root_.fabric._
            import _root_.fabric.rw._
            import _root_.scala.collection.immutable.VectorMap

            new ClassR[$tpe] {
              override protected def t2Map(t: $tpe): Map[String, Json] = VectorMap(..$allMap)
            }
           """)
      case None =>
        val caseObjects = companion.typeSignature.members.collect {
          case s: ModuleSymbol if s.moduleClass.asType.toType <:< tpe => s.name
        }.toList
        if (caseObjects.isEmpty) {
          context.abort(
            context.enclosingPosition,
            s"$t is not a valid case class (no primary constructor found) and no case objects detected!"
          )
        } else {
          context.Expr[Reader[T]](q"""
               import _root_.fabric._
               import _root_.fabric.rw._

               RW.enumeration[$t](List(..$caseObjects))
             """)
        }
    }
  }

  private def defaultsFor(
    context: blackbox.Context
  )(companion: context.Symbol): Map[Int, context.universe.MethodSymbol] = {
    import context.universe._
    val Default211RegexString = """[$]lessinit[$]greater[$]default[$](\d+)"""
    val DefaultRegexString = """apply[$]default[$](\d+)"""
    val Default211Regex = Default211RegexString.r
    val DefaultRegex = DefaultRegexString.r
    companion.typeSignature.decls.collect {
      case m: MethodSymbol if m.name.toString.matches(DefaultRegexString) =>
        m.name.toString match {
          case DefaultRegex(position) => (position.toInt - 1) -> m
          case s => throw new UnsupportedOperationException(s"Unable to parse: $s")
        }
      case m: MethodSymbol if m.name.toString.matches(Default211RegexString) =>
        m.name.toString match {
          case Default211Regex(position) => (position.toInt - 1) -> m
          case s => throw new UnsupportedOperationException(s"Unable to parse: $s")
        }
    }.toMap
  }

  private case class SerializedMemberInfo(jsonKey: String, memberName: String, returnType: Any)

  private def serializedMembers(
    context: blackbox.Context
  )(tpe: context.universe.Type): List[SerializedMemberInfo] = {
    import context.universe._
    tpe.members.collect {
      case m: MethodSymbol if m.annotations.exists(_.tree.tpe =:= typeOf[serialized]) =>
        val ann = m.annotations.find(_.tree.tpe =:= typeOf[serialized]).get
        val customName = ann.tree.children.tail.headOption match {
          case Some(Literal(Constant(name: String))) if name.nonEmpty => name
          case _ => m.name.decodedName.toString
        }
        val returnType = m.returnType.asSeenFrom(tpe, tpe.typeSymbol.asClass)
        SerializedMemberInfo(customName, m.name.decodedName.toString, returnType)
    }.toList
  }

  def caseClassW[T](
    context: blackbox.Context
  )(implicit t: context.WeakTypeTag[T]): context.Expr[Writer[T]] = {
    import context.universe._

    val tpe = t.tpe
    val isJsonWrapper: Boolean = tpe <:< typeOf[JsonWrapper]
    val companion: Symbol = tpe.typeSymbol.companion
    val defaults = defaultsFor(context)(companion)

    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) =>
        val fromMap: List[context.universe.Tree] = fields.zipWithIndex.map { case (field, index) =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          val returnType = tpe.decl(name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
          val (default, hasDefault) = defaults.get(index) match {
            case Some(m) => q"$companion.$m" -> true
            case None if returnType.resultType <:< typeOf[Option[_]] => q"""None""" -> true
            case None =>
              q"""throw RWException("Unable to find field " + ${tpe.toString} + "." + $key + " (and no defaults set) in " + Obj(map))""" -> false
          }
          if (key == "json" && isJsonWrapper) {
            q"json = Obj(map)"
          } else {
            q"""
               $name = map.get($key).orElse(map.collectFirst { case (k, v) if k.equalsIgnoreCase($key) => v }).map {
                 case Null if $hasDefault => $default
                 case json => json.as[$returnType]
               }.getOrElse($default)
             """
          }
        }
        context.Expr[Writer[T]](q"""
            import _root_.fabric._
            import _root_.fabric.rw._

            new ClassW[$tpe] {
              override protected def map2T(map: Map[String, Json]): $tpe = $companion(..$fromMap)
            }
           """)
      case None =>
        val caseObjects = companion.typeSignature.members.collect {
          case s: ModuleSymbol if s.moduleClass.asType.toType <:< tpe => s.name
        }.toList
        if (caseObjects.isEmpty) {
          context.abort(
            context.enclosingPosition,
            s"$t is not a valid case class (no primary constructor found)"
          )
        } else {
          context.Expr[Writer[T]](q"""
               import _root_.fabric._
               import _root_.fabric.rw._

               RW.enumeration[$t](List(..$caseObjects))
             """)
        }
    }
  }

  def caseClassRW[T](
    context: blackbox.Context
  )(implicit t: context.WeakTypeTag[T]): context.Expr[RW[T]] = {
    import context.universe._

    val tpe = t.tpe

    // AnyVal case class — proxy to the inner type's RW
    if (tpe <:< typeOf[AnyVal]) {
      tpe.decls.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
      } match {
        case Some(field :: Nil) =>
          val name = field.asTerm.name
          val innerType = tpe.decl(name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
          val companion = tpe.typeSymbol.companion
          val className = tpe.typeSymbol.fullName
          val genericTypes = generateGenericTypes(context)(tpe)
          return context.Expr[RW[T]](q"""
            import _root_.fabric._
            import _root_.fabric.rw._
            import _root_.fabric.define._

            new RW[$tpe] {
              private val innerRW = implicitly[RW[$innerType]]
              override def read(t: $tpe): Json = innerRW.read(t.$name)
              override def write(value: Json): $tpe = $companion(innerRW.write(value))
              override val definition: Definition = innerRW.definition.withClassName($className).copy(genericTypes = List(..$genericTypes))
            }
          """)
        case _ => // fall through to normal path
      }
    }

    val reader = caseClassR[T](context)
    val writer = caseClassW[T](context)
    val definition = caseClassD[T](context)
    context.Expr[RW[T]](q"""
         import _root_.fabric._
         import _root_.fabric.rw._
         import _root_.fabric.define._

         new RW[$tpe] {
            private val r = $reader
            private val w = $writer

            override def read(t: $tpe): Json = r.read(t)
            override def write(value: Json): $tpe = w.write(value)
            override def definition: Definition = $definition
         }
       """)
  }
}
