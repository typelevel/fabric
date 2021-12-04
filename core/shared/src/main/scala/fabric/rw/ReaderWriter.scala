package fabric.rw

import fabric._
import fabric.rw._

/**
 * ReaderWriter provides a single class representation of a Reader and Writer for the same type
 */
trait ReaderWriter[T] extends Reader[T] with Writer[T]

object ReaderWriter {
  implicit lazy val unitRW: ReaderWriter[Unit] = apply(_ => Null, _ => ())
  implicit lazy val valueRW: ReaderWriter[Value] = apply(identity, identity)

  implicit lazy val boolRW: ReaderWriter[Boolean] = apply[Boolean](bool, _.asBool.value)

  implicit lazy val shortRW: ReaderWriter[Short] = apply[Short](s => num(s.toDouble), _.asNum.asShort)
  implicit lazy val intRW: ReaderWriter[Int] = apply[Int](i => num(i), _.asNum.asInt)
  implicit lazy val longRW: ReaderWriter[Long] = apply[Long](l => num(l), _.asNum.asLong)
  implicit lazy val floatRW: ReaderWriter[Float] = apply[Float](f => num(f.toDouble), _.asNum.asFloat)
  implicit lazy val doubleRW: ReaderWriter[Double] = apply[Double](num, _.asNum.asDouble)
  implicit lazy val bigIntRW: ReaderWriter[BigInt] = apply[BigInt](i => num(BigDecimal(i)), _.asNum.asBigInt)
  implicit lazy val bigDecimalRW: ReaderWriter[BigDecimal] = apply[BigDecimal](num, _.asNum.asBigDecimal)

  implicit lazy val stringRW: ReaderWriter[String] = apply[String](str, _.asStr.value)

  implicit lazy val stringMapRW: ReaderWriter[Map[String, String]] = apply[Map[String, String]](_.map {
    case (key, value) => key -> str(value)
  }, v => v.asObj.value.map {
    case (key, value) => key -> value.asStr.value
  })

  def apply[T](r: T => Value, w: Value => T): ReaderWriter[T] = new ReaderWriter[T] {
    override def write(value: Value): T = w(value)

    override def read(t: T): Value = r(t)
  }
}