package fabric.rw

import fabric._
import fabric.rw._

/**
 * RW provides a single class representation of a Reader and Writer for the same type
 */
trait RW[T] extends Reader[T] with Writer[T]

object RW extends CompileRW {
  implicit lazy val unitRW: RW[Unit] = from(_ => Null, _ => ())
  implicit lazy val valueRW: RW[Json] = from(identity, identity)
  implicit lazy val objRW: RW[Obj] = from(o => o, v => v.asObj)

  implicit lazy val boolRW: RW[Boolean] = from[Boolean](bool, _.asBool.value)

  implicit lazy val byteRW: RW[Byte] = from[Byte](s => NumInt(s.toInt), _.asNum.asByte)
  implicit lazy val shortRW: RW[Short] = from[Short](s => num(s.toInt), _.asNum.asShort)
  implicit lazy val intRW: RW[Int] = from[Int](i => num(i), _.asNum.asInt)
  implicit lazy val longRW: RW[Long] = from[Long](l => num(l), _.asNum.asLong)
  implicit lazy val floatRW: RW[Float] = from[Float](f => num(f.toDouble), _.asNum.asFloat)
  implicit lazy val doubleRW: RW[Double] = from[Double](num, _.asNum.asDouble)
  implicit lazy val bigIntRW: RW[BigInt] = from[BigInt](i => num(BigDecimal(i)), _.asNum.asBigInt)
  implicit lazy val bigDecimalRW: RW[BigDecimal] = from[BigDecimal](num, _.asNum.asBigDecimal)

  implicit lazy val stringRW: RW[String] = from[String](str, _.asStr.value)

  implicit def mapRW[V: RW]: RW[Map[String, V]] = from[Map[String, V]](_.map {
    case (key, value) => key -> value.json
  }, v => v.asObj.value.map {
    case (key, value) => key -> value.as[V]
  })

  implicit def listRW[V: RW]: RW[List[V]] = from[List[V]](
    v => Arr(v.map(_.json).toVector),
    v => v.asVector.map(_.as[V]).toList
  )

  implicit def vectorRW[V: RW]: RW[Vector[V]] = from[Vector[V]](
    v => Arr(v.map(_.json)),
    v => v.asVector.map(_.as[V])
  )

  implicit def setRW[V: RW]: RW[Set[V]] = from[Set[V]](
    v => Arr(v.map(_.json).toVector),
    {
      case Arr(vector) => vector.map(_.as[V]).toSet
      case v => throw new RuntimeException(s"Unsupported set: $v")
    }
  )

  implicit def optionRW[V: RW]: RW[Option[V]] = from[Option[V]](
    v => v.map(_.json).getOrElse(Null),
    v => if (v.isNull) None else Some(v.as[V])
  )

  def from[T](r: T => Json, w: Json => T): RW[T] = new RW[T] {
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
  def static[T](value: T): RW[T] = from(_ => obj(), _ => value)

  /**
   * Convenience functionality for working with enumerations
   *
   * @param fieldName the field name to refer to in the Json
   * @param mapping   a mapping of key/value pairs representing the String in fieldName to the representative value
   */
  def enumeration[T](fieldName: String, mapping: (String, T)*): RW[T] = {
    val f2T: Map[String, T] = mapping.toMap
    val t2F: Map[T, String] = f2T.map(_.swap)

    from(
      (t: T) => obj(fieldName -> t2F(t)),
      (v: Json) => f2T(v.asObj.value(fieldName).asStr.value)
    )
  }

  /**
   * Convenience functionality for working with polymorphic types
   *
   * @param fieldName the field name stored in the value (defaults to "type")
   * @param getType   a function to determine the field value from an instance (defaults to the class name with the first character lowercase - defaultGetType)
   * @param matcher   a matcher for field values to get the representative RW for that type
   */
  def poly[P](fieldName: String = "type",
              getType: P => String = defaultGetType _)
             (matcher: PartialFunction[String, RW[_ <: P]]): RW[P] = from(
    p => {
      val `type` = getType(p)
      if (matcher.isDefinedAt(`type`)) {
        matcher(`type`).asInstanceOf[RW[P]].read(p).merge(obj(fieldName -> `type`))
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