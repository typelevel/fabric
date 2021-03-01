package hierarchical.rw

import hierarchical._
import hierarchical.rw._

/**
 * ReadableWritable provides a single class representation of a Readable and Writable for the same type
 */
trait ReadableWritable[T] extends Readable[T] with Writable[T]

object ReadableWritable {
  implicit lazy val boolRW: ReadableWritable[Boolean] = apply[Boolean](bool, _.asBool.value)

  implicit lazy val shortRW: ReadableWritable[Short] = apply[Short](s => num(s.toDouble), _.asNum.asShort)
  implicit lazy val intRW: ReadableWritable[Int] = apply[Int](i => num(i.toDouble), _.asNum.asInt)
  implicit lazy val longRW: ReadableWritable[Long] = apply[Long](l => num(l.toDouble), _.asNum.asLong)
  implicit lazy val floatRW: ReadableWritable[Float] = apply[Float](f => num(f.toDouble), _.asNum.asFloat)
  implicit lazy val doubleRW: ReadableWritable[Double] = apply[Double](num, _.asNum.asDouble)
  implicit lazy val bigIntRW: ReadableWritable[BigInt] = apply[BigInt](i => num(BigDecimal(i)), _.asNum.asBigInt)
  implicit lazy val bigDecimalRW: ReadableWritable[BigDecimal] = apply[BigDecimal](num, _.asNum.asBigDecimal)

  implicit lazy val stringRW: ReadableWritable[String] = apply[String](str, _.asStr.value)

  def apply[T](r: T => Value, w: Value => T): ReadableWritable[T] = new ReadableWritable[T] {
    override def write(value: Value): T = w(value)

    override def read(t: T): Value = r(t)
  }
}