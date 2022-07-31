package fabric.rw

import fabric._

/**
 * Writable provides a simple Value => T wrapper functionality
 */
trait Writer[T] {
  def write(value: Json): T
}

object Writer {
  import ReaderWriter._

  implicit def unitW: Writer[Unit] = unitRW
  implicit def valueW: Writer[Json] = valueRW
  implicit def objW: Writer[Obj] = objRW

  implicit def boolW: Writer[Boolean] = boolRW

  implicit def byteR: Writer[Byte] = byteRW
  implicit def shortR: Writer[Short] = shortRW
  implicit def intW: Writer[Int] = intRW
  implicit def longW: Writer[Long] = longRW
  implicit def floatW: Writer[Float] = floatRW
  implicit def doubleW: Writer[Double] = doubleRW
  implicit def bigIntW: Writer[BigInt] = bigIntRW
  implicit def bigDecimalW: Writer[BigDecimal] = bigDecimalRW

  implicit def stringW: Writer[String] = stringRW
  implicit def mapW[V: Writer]: Writer[Map[String, V]] = apply[Map[String, V]] { v =>
    v.asObj.value.map {
      case (key, value) => key -> value.as[V]
    }
  }
  implicit def listW[V: Writer]: Writer[List[V]] = apply[List[V]](
    v => v.asVector.map(_.as[V]).toList
  )
  implicit def vectorW[V: Writer]: Writer[Vector[V]] = apply[Vector[V]](
    v => v.asVector.map(_.as[V])
  )
  implicit def setW[T](implicit w: Writer[T]): Writer[Set[T]] = apply[Set[T]] {
    case Arr(vector) => vector.toSet.map(w.write)
    case v => throw new RuntimeException(s"Unsupported set: $v")
  }
  implicit def optionW[T](implicit w: Writer[T]): Writer[Option[T]] = apply[Option[T]] {
    case Null => None
    case v => Option(w.write(v))
  }

  def apply[T](f: Json => T): Writer[T] = new Writer[T] {
    override def write(value: Json): T = f(value)
  }
}