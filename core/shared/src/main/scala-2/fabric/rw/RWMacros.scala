package fabric.rw

import scala.reflect.macros.blackbox

object RWMacros {
  def caseClass[T](context: blackbox.Context)
                  (implicit t: context.WeakTypeTag[T]): context.Expr[ReaderWriter[T]] = {
    import context.universe._

    val tpe = t.tpe
    val companion = tpe.typeSymbol.companion
    val DefaultRegex = """apply[$]default[$](\d+)""".r
    val defaults: Map[Int, context.universe.MethodSymbol] = companion.typeSignature.decls.collect {
      case m: MethodSymbol => m.name.toString match {
        case DefaultRegex(position) => Some((position.toInt - 1) -> m)
        case _ => None
      }
    }.flatten.toMap
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) => {
        val (toMap, fromMap) = fields.zipWithIndex.map {
          case (field, index) => {
            val name = field.asTerm.name
            val key = name.decodedName.toString
            val returnType = tpe.decl(name).typeSignature
            val default = defaults.get(index) match {
              case Some(m) => q"$companion.$m"
              case None => q"""throw new RuntimeException("Unable to find field " + ${tpe.toString} + "." + $key + " (and no defaults set) in " + Obj(map))"""
            }
            val toMap = q"$key -> t.$name.toValue"
            val fromMap = q"""$name = map.get($key).map(_.as[$returnType]).getOrElse($default)"""
            (toMap, fromMap)
          }
        }.unzip
        context.Expr[ReaderWriter[T]](
          q"""
            import _root_.fabric._
            import _root_.fabric.rw._

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
