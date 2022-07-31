package fabric.rw

import fabric._
import fabric.rw._

/**
 * ReaderWriter provides a single class representation of a Reader and Writer for the same type
 */
trait ReaderWriter[T] extends Reader[T] with Writer[T]

object ReaderWriter {
  implicit lazy val unitRW: ReaderWriter[Unit] = apply(_ => Null, _ => ())
  implicit lazy val valueRW: ReaderWriter[Json] = apply(identity, identity)
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

  implicit def listRW[V: ReaderWriter]: ReaderWriter[List[V]] = apply[List[V]](
    v => Arr(v.map(_.toValue).toVector),
    v => v.asVector.map(_.as[V]).toList
  )

  implicit def vectorRW[V: ReaderWriter]: ReaderWriter[Vector[V]] = apply[Vector[V]](
    v => Arr(v.map(_.toValue)),
    v => v.asVector.map(_.as[V])
  )

  implicit def setRW[V: ReaderWriter]: ReaderWriter[Set[V]] = apply[Set[V]](
    v => Arr(v.map(_.toValue).toVector),
    {
      case Arr(vector) => vector.map(_.as[V]).toSet
      case v => throw new RuntimeException(s"Unsupported set: $v")
    }
  )

  implicit def optionRW[V: ReaderWriter]: ReaderWriter[Option[V]] = apply[Option[V]](
    v => v.map(_.toValue).getOrElse(Null),
    v => if (v.isNull) None else Some(v.as[V])
  )

  def apply[T](r: T => Json, w: Json => T): ReaderWriter[T] = new ReaderWriter[T] {
    override def write(value: Json): T = w(value)

    override def read(t: T): Json = r(t)
  }

  def enumeration[T](list: List[T],
                     asString: T => String = (t: T) => t.getClass.getSimpleName.replace("$", ""),
                     caseSensitive: Boolean = false): RW[T] = new RW[T] {
    private def fixString(s: String): String = if (caseSensitive) s else s.toLowerCase

    private lazy val map = list.map(t => fixString(asString(t)) -> t).toMap

    override def write(value: Json): T = map(fixString(value.asString))

    override def read(t: T): Json = str(asString(t))
  }

  def string[T](asString: T => String, fromString: String => T): RW[T] = new RW[T] {
    override def write(value: Json): T = fromString(value.asString)

    override def read(t: T): Json = str(asString(t))
  }
}