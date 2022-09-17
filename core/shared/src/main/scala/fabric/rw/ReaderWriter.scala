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
    case (key, value) => key -> value.json
  }, v => v.asObj.value.map {
    case (key, value) => key -> value.as[V]
  })

  implicit def listRW[V: ReaderWriter]: ReaderWriter[List[V]] = apply[List[V]](
    v => Arr(v.map(_.json).toVector),
    v => v.asVector.map(_.as[V]).toList
  )

  implicit def vectorRW[V: ReaderWriter]: ReaderWriter[Vector[V]] = apply[Vector[V]](
    v => Arr(v.map(_.json)),
    v => v.asVector.map(_.as[V])
  )

  implicit def setRW[V: ReaderWriter]: ReaderWriter[Set[V]] = apply[Set[V]](
    v => Arr(v.map(_.json).toVector),
    {
      case Arr(vector) => vector.map(_.as[V]).toSet
      case v => throw new RuntimeException(s"Unsupported set: $v")
    }
  )

  implicit def optionRW[V: ReaderWriter]: ReaderWriter[Option[V]] = apply[Option[V]](
    v => v.map(_.json).getOrElse(Null),
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

  /**
   * Convenience functionality to provide a static / singleton value that represents that type
   *
   * @param value the singleton value to use
   */
  def static[T](value: T): ReaderWriter[T] = ReaderWriter(_ => obj(), _ => value)

  /**
   * Convenience functionality for working with enumerations
   *
   * @param fieldName the field name to refer to in the Value
   * @param mapping   a mapping of key/value pairs representing the String in fieldName to the representative value
   */
  def enumeration[T](fieldName: String, mapping: (String, T)*): ReaderWriter[T] = {
    val f2T: Map[String, T] = mapping.toMap
    val t2F: Map[T, String] = f2T.map(_.swap)

    ReaderWriter(
      (t: T) => obj(fieldName -> t2F(t)),
      (v: Json) => f2T(v.asObj.value(fieldName).asStr.value)
    )
  }

  /**
   * Convenience functionality for working with polymorphic types
   *
   * @param fieldName the field name stored in the value (defaults to "type")
   * @param getType   a function to determine the field value from an instance (defaults to the class name with the first character lowercase - defaultGetType)
   * @param matcher   a matcher for field values to get the representative ReaderWriter for that type
   */
  def poly[P](fieldName: String = "type",
              getType: P => String = defaultGetType _)
             (matcher: PartialFunction[String, ReaderWriter[_ <: P]]): ReaderWriter[P] = ReaderWriter(
    p => {
      val `type` = getType(p)
      if (matcher.isDefinedAt(`type`)) {
        matcher(`type`).asInstanceOf[ReaderWriter[P]].read(p).merge(obj(fieldName -> `type`))
      } else {
        throw new RuntimeException(s"Type not found [${`type`}] converting from object $p")
      }
    },
    v => {
      val `type` = v(fieldName).asStr.value
      if (matcher.isDefinedAt(`type`)) {
        matcher(`type`).write(v)
      } else {
        throw new RuntimeException(s"Type not found [${`type`}] converting from value $v")
      }
    }
  )

  /**
   * Used by poly by default to getType using the class name with the first character lowercase
   */
  private def defaultGetType[P](p: P): String = {
    val name = p.getClass.getSimpleName.replace("$", "")
    s"${name.charAt(0).toLower}${name.substring(1)}"
  }
}