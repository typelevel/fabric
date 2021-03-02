package hierarchical.rw

import hierarchical.*

import scala.deriving.*
import scala.compiletime.*

trait CompileRW {
  inline def ccRW[T <: Product](using Mirror.ProductOf[T]): ReaderWriter[T] = new ClassRW[T] {
    override protected def t2Map(t: T): Map[String, Value] = toMap(t)
    override protected def map2T(map: Map[String, Value]): T = fromMap[T](map)
  }

  inline def toMap[T <: Product](t: T)(using p: Mirror.ProductOf[T]): Map[String, Value] = {
    toMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](t, 0)
  }

  inline def toMapElems[A <: Product, T <: Tuple, L <: Tuple](a: A, index: Int): Map[String, Value] = {
    inline erasedValue[T] match
      case _: (hd *: tl) =>
        inline erasedValue[L] match
          case _: (hdLabel *: tlLabels) =>
            import hierarchical.rw.given
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValue = a.productElement(index).asInstanceOf[hd]
            val hdReader = summonInline[Reader[hd]]
            val value = hdReader.read(hdValue)
            toMapElems[A, tl, tlLabels](a, index + 1) ++ Map(hdLabelValue -> value)
          case EmptyTuple => sys.error("Not possible")
      case EmptyTuple => Map.empty
  }

  inline def fromMap[T <: Product](map: Map[String, Value])(using p: Mirror.ProductOf[T]): T = {
    inline val size = constValue[Tuple.Size[p.MirroredElemTypes]]
    val arr = new Array[Any](size)
    fromMapElems[T, p.MirroredElemTypes, p.MirroredElemLabels](map, 0, arr)
    val product: Product = new Product {
      override def canEqual(that: Any): Boolean = true
      override def productArity: Int = arr.size
      override def productElement(n: Int): Any = arr(n)
    }
    p.fromProduct(product)
  }

  inline def fromMapElems[A <: Product, T <: Tuple, L <: Tuple](map: Map[String, Value], index: Int, arr: Array[Any]): Unit = {
    inline erasedValue[T] match
      case _: (hd *: tl) =>
        inline erasedValue[L] match
          case _: (hdLabel *: tlLabels) =>
            import hierarchical.rw.given
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdValue = map(hdLabelValue)
            val hdWritable = summonInline[Writer[hd]]
            val value = hdWritable.write(hdValue)
            arr(index) = value
            fromMapElems[A, tl, tlLabels](map, index + 1, arr)
          case EmptyTuple => sys.error("Not possible")
      case EmptyTuple => // Finished
  }
}