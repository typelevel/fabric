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

import scala.reflect.macros.blackbox

object RWMacros {
  def caseClassR[T](context: blackbox.Context)(implicit t: context.WeakTypeTag[T]): context.Expr[Reader[T]] = {
    import context.universe._

    val tpe = t.tpe
    val companion = tpe.typeSymbol.companion
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) => {
        val toMap = fields.map { field =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          q"$key -> t.$name.json"
        }
        context.Expr[Reader[T]](
          q"""
            import _root_.fabric._
            import _root_.fabric.rw._
            import _root_.scala.collection.immutable.ListMap

            new ClassR[$tpe] {
              override protected def t2Map(t: $tpe): ListMap[String, Json] = ListMap(..$toMap)
            }
           """)
      }
      case None =>
        val caseObjects = tpe.companion.members.collect {
          case s if s.typeSignature <:< t.tpe => s.name
        }
        if (caseObjects.isEmpty) {
          context.abort(context.enclosingPosition, s"$t is not a valid case class (no primary constructor found)")
        } else {
          context.Expr[Reader[T]](
            q"""
               import _root_.fabric._
               import _root_.fabric.rw._
               import _root_.scala.collection.immutable.ListMap

               RW.enumeration[$t](List(..$caseObjects))
             """
          )
        }
    }
  }

  def caseClassW[T](context: blackbox.Context)(implicit t: context.WeakTypeTag[T]): context.Expr[Writer[T]] = {
    import context.universe._

    val tpe = t.tpe
    val companion = tpe.typeSymbol.companion
    val Default211RegexString = """[$]lessinit[$]greater[$]default[$](\d+)"""
    val DefaultRegexString = """apply[$]default[$](\d+)"""
    val Default211Regex = Default211RegexString.r
    val DefaultRegex = DefaultRegexString.r
    val defaults: Map[Int, context.universe.MethodSymbol] = companion.typeSignature.decls.collect {
      case m: MethodSymbol if m.name.toString.matches(DefaultRegexString) => m.name.toString match {
        case DefaultRegex(position) => (position.toInt - 1) -> m
      }
      case m: MethodSymbol if m.name.toString.matches(Default211RegexString) => m.name.toString match {
        case Default211Regex(position) => (position.toInt - 1) -> m
      }
    }.toMap
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) => {
        val fromMap = fields.zipWithIndex.map {
          case (field, index) =>
            val name = field.asTerm.name
            val key = name.decodedName.toString
            val returnType = tpe.decl(name).typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)
            val default = defaults.get(index) match {
              case Some(m) => q"$companion.$m"
              case None if returnType.resultType <:< typeOf[Option[_]] => q"""None"""
              case None => q"""sys.error("Unable to find field " + ${tpe.toString} + "." + $key + " (and no defaults set) in " + Obj(map))"""
            }
            q"""$name = map.get($key).map(_.as[$returnType]).getOrElse($default)"""
        }
        context.Expr[Writer[T]](
          q"""
            import _root_.fabric._
            import _root_.fabric.rw._
            import _root_.scala.collection.immutable.ListMap

            new ClassW[$tpe] {
              override protected def map2T(map: ListMap[String, Json]): $tpe = $companion(..$fromMap)
            }
           """)
      }
      case None =>
        val caseObjects = tpe.companion.members.collect {
          case s if s.typeSignature <:< t.tpe => s.name
        }
        if (caseObjects.isEmpty) {
          context.abort(context.enclosingPosition, s"$t is not a valid case class (no primary constructor found)")
        } else {
          context.Expr[Writer[T]](
            q"""
               import _root_.fabric._
               import _root_.fabric.rw._
               import _root_.scala.collection.immutable.ListMap

               RW.enumeration[$t](List(..$caseObjects))
             """
          )
        }
    }
  }

  def caseClassRW[T](context: blackbox.Context)
                  (implicit t: context.WeakTypeTag[T]): context.Expr[RW[T]] = {
    import context.universe._

    val tpe = t.tpe
    val reader = caseClassR[T](context)
    val writer = caseClassW[T](context)
    context.Expr[RW[T]](
      q"""
         import _root_.fabric._
         import _root_.fabric.rw._

         new RW[$tpe] {
            private val r = $reader
            private val w = $writer

            override def read(t: $tpe): Json = r.read(t)
            override def write(value: Json): $tpe = w.write(value)
         }
       """
    )
  }
}