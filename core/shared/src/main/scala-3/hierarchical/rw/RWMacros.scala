package hierarchical.rw

import scala.compiletime._
import scala.quoted._

object RWMacros {
  /*def caseClass[T: Type](using quotes: Quotes): Expr[ReadableWritable[T]] = {
    val t = Tuple.fromProductTyped()

//    import quotes.reflect._
//
//    println(s"Quotes: $quotes")
//    val target = TypeRepr.of[T]
//    val sym = target.classSymbol.get
//    val fields = sym.declaredFields
//    val caseFields = sym.caseFields.filter(fields.contains)
//    println(s"Sym: $sym / $caseFields")
//    caseFields.foreach { f =>
//      val name = f.name
//      val memberType = target.memberType(f)
////      val toMap = '{name -> t.$name}
//    }
//
//    '{
//      import hierarchical._
//      import hierarchical.rw._
//
//      new ClassRW[T] {
//        override protected def t2Map(t: T): Map[String, Value] = ???
//        override protected def map2T(map: Map[String, Value]): T = ???
//      }
//    }
  }*/
}