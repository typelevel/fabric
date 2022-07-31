package fabric.rw

import scala.reflect.macros.blackbox

object RWMacros {
  def caseClassR[T](context: blackbox.Context)(implicit t: context.WeakTypeTag[T]): context.Expr[Reader[T]] = {
    import context.universe._

    val tpe = t.tpe
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m.paramLists.head
    } match {
      case Some(fields) => {
        val toMap = fields.map { field =>
          val name = field.asTerm.name
          val key = name.decodedName.toString
          q"$key -> t.$name.toValue"
        }
        context.Expr[Reader[T]](
          q"""
            import _root_.fabric._
            import _root_.fabric.rw._

            new ClassR[$tpe] {
              override protected def t2Map(t: $tpe): Map[String, Json] = Map(..$toMap)
            }
           """)
      }
      case None => context.abort(context.enclosingPosition, s"$t is not a valid case class (no primary constructor found)")
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
          case (field, index) => {
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
        }
        context.Expr[Writer[T]](
          q"""
            import _root_.fabric._
            import _root_.fabric.rw._

            new ClassW[$tpe] {
              override protected def map2T(map: Map[String, Json]): $tpe = $companion(..$fromMap)
            }
           """)
      }
      case None => context.abort(context.enclosingPosition, s"$t is not a valid case class (no primary constructor found)")
    }
  }

  def caseClassRW[T](context: blackbox.Context)
                  (implicit t: context.WeakTypeTag[T]): context.Expr[ReaderWriter[T]] = {
    import context.universe._

    val tpe = t.tpe
    val reader = caseClassR[T](context)
    val writer = caseClassW[T](context)
    context.Expr[ReaderWriter[T]](
      q"""
         import _root_.fabric._
         import _root_.fabric.rw._

         new ReaderWriter[$tpe] {
            private val r = $reader
            private val w = $writer

            override def read(t: $tpe): Json = r.read(t)
            override def write(value: Json): $tpe = w.write(value)
         }
       """
    )
  }
}