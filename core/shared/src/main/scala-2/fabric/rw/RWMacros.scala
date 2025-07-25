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
import fabric.define.DefType

import scala.reflect.macros.blackbox

object RWMacros {
  def caseClassD[T](
    context: blackbox.Context
  )(implicit t: context.WeakTypeTag[T]): context.Expr[DefType] = {
    import context.universe._

    val tpe = t.tpe
    val className = tpe.typeSymbol.asClass.fullName
    val companion: Symbol = tpe.typeSymbol.companion
    val defaults = defaultsFor(context)(companion)
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) =>
        val fieldDefs = fields.zipWithIndex.map { case (field, index) =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          val returnType = tpe.decl(name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
          if (defaults.contains(index)) {
            q"$key -> implicitly[RW[$returnType]].definition.opt"
          } else {
            q"$key -> implicitly[RW[$returnType]].definition"
          }
        }
        context.Expr[DefType](q"""
            import _root_.fabric._
            import _root_.fabric.define._

            DefType.Obj(Some($className), ..$fieldDefs)
           """)
      case None =>
        val caseObjects = companion.typeSignature.members.collect {
          case s: ModuleSymbol if s.moduleClass.asType.toType <:< tpe => s.name
        }.toList

        if (caseObjects.isEmpty) {
          context.abort(context.enclosingPosition, "Not a valid case class or sealed trait with case objects")
        } else {
          // Generate a DefType for an enumeration by delegating to RW.enumeration
          context.Expr[DefType](q"""
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
        val toMap: List[context.universe.Tree] = fields.map { field =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          q"$key -> t.$name.json"
        }
        context.Expr[Reader[T]](q"""
            import _root_.fabric._
            import _root_.fabric.rw._
            import _root_.scala.collection.immutable.VectorMap

            new ClassR[$tpe] {
              override protected def t2Map(t: $tpe): Map[String, Json] = VectorMap(..$toMap)
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
               $name = map.get($key).map {
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
            override def definition: DefType = $definition
         }
       """)
  }
}
