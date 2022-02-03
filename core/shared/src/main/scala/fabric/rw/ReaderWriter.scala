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
  implicit lazy val objRW: ReaderWriter[Obj] = apply(o => o, v => v.asObj)

  implicit lazy val boolRW: ReaderWriter[Boolean] = apply[Boolean](bool, _.asBool.value)

  implicit lazy val byteRW: ReaderWriter[Byte] = apply[Byte](s => NumInt(s.toInt), _.asNum.asByte)
  implicit lazy val shortRW: ReaderWriter[Short] = apply[Short](s => num(s.toInt), _.asNum.asShort)
  implicit lazy val intRW: ReaderWriter[Int] = apply[Int](i => num(i), _.asNum.asInt)
  implicit lazy val longRW: ReaderWriter[Long] = apply[Long](l => num(l), _.asNum.asLong)
  implicit lazy val floatRW: ReaderWriter[Float] = apply[Float](f => num(f.toDouble), _.asNum.asFloat)
  implicit lazy val doubleRW: ReaderWriter[Double] = apply[Double](num, _.asNum.asDouble)
  implicit lazy val bigIntRW: ReaderWriter[BigInt] = apply[BigInt](i => num(BigDecimal(i)), _.asNum.asBigInt)
  implicit lazy val bigDecimalRW: ReaderWriter[BigDecimal] = apply[BigDecimal](num, _.asNum.asBigDecimal)

  implicit lazy val stringRW: ReaderWriter[String] = apply[String](str, _.asStr.value)

  implicit def mapRW[V: ReaderWriter]: ReaderWriter[Map[String, V]] = apply[Map[String, V]](_.map {
    case (key, value) => key -> value.toValue
  }, v => v.asObj.value.map {
    case (key, value) => key -> value.as[V]
  })

  def apply[T](r: T => Value, w: Value => T): ReaderWriter[T] = new ReaderWriter[T] {
    override def write(value: Value): T = w(value)

    override def read(t: T): Value = r(t)
  }

  def enumeration[T](list: List[T], asString: T => String): RW[T] = new RW[T] {
    private lazy val map = list.map(t => asString(t) -> t).toMap

    override def write(value: Value): T = map(value.asString)

    override def read(t: T): Value = str(asString(t))
  }

  def string[T](asString: T => String, fromString: String => T): RW[T] = new RW[T] {
    override def write(value: Value): T = fromString(value.asString)

    override def read(t: T): Value = str(asString(t))
  }
}