package hierarchical.rw

import scala.reflect.macros.blackbox

object RWMacros {
  def caseClass[T](context: blackbox.Context)
                  (implicit t: context.WeakTypeTag[T]): context.Expr[ReaderWriter[T]] = {
    import context.universe._

    val tpe = t.tpe
    val companion = tpe.typeSymbol.companion
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) => {
        val (toMap, fromMap) = fields.map { field =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          val returnType = tpe.decl(name).typeSignature
          val toMap = q"$key -> t.$name.toValue"
          val fromMap = q"$name = map($key).as[$returnType]"
          (toMap, fromMap)
        }.unzip
        context.Expr[ReaderWriter[T]](
          q"""
            import _root_.hierarchical._
            import _root_.hierarchical.rw._

            new ClassRW[$tpe] {
              override protected def t2Map(t: $tpe): Map[String, Value] = Map(..$toMap)
              override protected def map2T(map: Map[String, Value]): $tpe = $companion(..$fromMap)
            }
           """)
      }
      case None => context.abort(context.enclosingPosition, s"$t is not a valid case class (no primary constructor found)")
    }
  }
}
