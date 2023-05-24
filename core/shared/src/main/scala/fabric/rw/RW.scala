/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.rw

import fabric._
import fabric.define.DefType

import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.util.matching.Regex

/**
  * RW provides a single class representation of a Reader and Writer for the
  * same type
  */
trait RW[T] extends Reader[T] with Writer[T] {
  def definition: DefType

  override def +(that: Reader[T]): RW[T] = CompoundRW[T](super.+(that), this, definition)

  override def +(that: Writer[T])(implicit merge: (T, T) => T): RW[T] = CompoundRW[T](this, super.+(that), definition)
}

object RW extends CompileRW {
  implicit lazy val unitRW: RW[Unit] = from(_ => Null, _ => (), DefType.Null)
  implicit lazy val valueRW: RW[Json] = from(identity, identity, DefType.Dynamic)
  implicit lazy val objRW: RW[Obj] = from(o => o, v => v.asObj, DefType.Dynamic)

  implicit lazy val boolRW: RW[Boolean] = from[Boolean](bool, _.asBool.value, DefType.Bool)

  implicit lazy val byteRW: RW[Byte] = from[Byte](s => NumInt(s.toLong), _.asNum.asByte, DefType.Int)
  implicit lazy val shortRW: RW[Short] = from[Short](s => num(s.toInt), _.asNum.asShort, DefType.Int)
  implicit lazy val intRW: RW[Int] = from[Int](i => num(i), _.asNum.asInt, DefType.Int)
  implicit lazy val longRW: RW[Long] = from[Long](l => num(l), _.asNum.asLong, DefType.Int)
  implicit lazy val floatRW: RW[Float] = from[Float](f => num(f.toDouble), _.asNum.asFloat, DefType.Dec)
  implicit lazy val doubleRW: RW[Double] = from[Double](num, _.asNum.asDouble, DefType.Dec)
  implicit lazy val bigIntRW: RW[BigInt] = from[BigInt](i => num(BigDecimal(i)), _.asNum.asBigInt, DefType.Dec)
  implicit lazy val bigDecimalRW: RW[BigDecimal] = from[BigDecimal](num, _.asNum.asBigDecimal, DefType.Dec)

  implicit lazy val stringRW: RW[String] = from[String](str, _.asStr.value, DefType.Str)

  implicit lazy val regexRW: RW[Regex] = string[Regex](_.toString(), _.r)

  implicit lazy val finiteDurationRW: RW[FiniteDuration] = from[FiniteDuration](
    r = fd => fd.toMillis.json,
    w = j => j.asLong.millis,
    d = DefType.Int
  )

  implicit def mapRW[K, V](implicit keyRW: RW[K], valueRW: RW[V]): RW[Map[K, V]] =
    if (keyRW.definition == DefType.Str) {
      from[Map[K, V]](
        _.map { case (key, value) => key.json.asString -> value.json },
        _.asObj.value.map { case (key, value) => str(key).as[K] -> value.as[V] },
        DefType.Dynamic
      )
    } else {
      val reader = Reader.mapR[K, V]
      val writer = Writer.mapW[K, V]
      from[Map[K, V]](
        r = reader.read,
        w = writer.write,
        d = DefType.Arr(
          DefType.Obj(
            "key" -> implicitly[RW[K]].definition,
            "value" -> implicitly[RW[V]].definition
          )
        )
      )
    }

  implicit def tuple2RW[K: RW, V: RW]: RW[(K, V)] = from[(K, V)](
    r = t => arr(t._1.json, t._2.json),
    w = j =>
      j.asVector match {
        case Vector(k, v) => (k.as[K], v.as[V])
        case v => throw new RuntimeException(s"Invalid shape for tuple2: $v")
      },
    d = DefType.Arr(DefType.Dynamic)
  )

  implicit def tuple3RW[T1: RW, T2: RW, T3: RW]: RW[(T1, T2, T3)] = from[(T1, T2, T3)](
    r = t => arr(t._1.json, t._2.json, t._3.json),
    w = j =>
      j.asVector match {
        case Vector(t1, t2, t3) => (t1.as[T1], t2.as[T2], t3.as[T3])
        case v => throw new RuntimeException(s"Invalid shape for tuple3: $v")
      },
    d = DefType.Arr(DefType.Dynamic)
  )

  implicit def tuple4RW[T1: RW, T2: RW, T3: RW, T4: RW]: RW[(T1, T2, T3, T4)] = from[(T1, T2, T3, T4)](
    r = t => arr(t._1.json, t._2.json, t._3.json, t._4.json),
    w = j =>
      j.asVector match {
        case Vector(t1, t2, t3, t4) => (t1.as[T1], t2.as[T2], t3.as[T3], t4.as[T4])
        case v => throw new RuntimeException(s"Invalid shape for tuple4: $v")
      },
    d = DefType.Arr(DefType.Dynamic)
  )

  implicit def listRW[V: RW]: RW[List[V]] = from[List[V]](
    v => Arr(v.map(_.json).toVector),
    v => v.asVector.map(_.as[V]).toList,
    DefType.Arr(implicitly[RW[V]].definition)
  )

  implicit def vectorRW[V: RW]: RW[Vector[V]] = from[Vector[V]](
    v => Arr(v.map(_.json)),
    v => v.asVector.map(_.as[V]),
    DefType.Arr(implicitly[RW[V]].definition)
  )

  implicit def setRW[V: RW]: RW[Set[V]] = from[Set[V]](
    v => Arr(v.map(_.json).toVector),
    {
      case Arr(vector) => vector.map(_.as[V]).toSet
      case v => throw new RuntimeException(s"Unsupported set: $v")
    },
    DefType.Arr(implicitly[RW[V]].definition)
  )

  implicit def optionRW[V: RW]: RW[Option[V]] = from[Option[V]](
    v => v.map(_.json).getOrElse(Null),
    v => if (v.isNull) None else Some(v.as[V]),
    DefType.Opt(implicitly[RW[V]].definition)
  )

  def from[T](r: T => Json, w: Json => T, d: => DefType): RW[T] = new RW[T] {
    override def write(value: Json): T = w(value)

    override def read(t: T): Json = r(t)

    override def definition: DefType = d
  }

  def enumeration[T](
    list: List[T],
    asString: T => String = (t: T) => t.getClass.getSimpleName.replace("$", ""),
    caseSensitive: Boolean = false
  ): RW[T] = new RW[T] {
    private def fixString(s: String): String = if (caseSensitive) s else s.toLowerCase

    private lazy val map = list.map(t => fixString(asString(t)) -> t).toMap

    override def write(value: Json): T = map(fixString(value.asString))

    override def read(t: T): Json = str(asString(t))

    override val definition: DefType = DefType.Enum(list.map(t => Str(asString(t))))
  }

  def string[T](asString: T => String, fromString: String => T): RW[T] = new RW[T] {
    override def write(value: Json): T = fromString(value.asString)

    override def read(t: T): Json = str(asString(t))

    override def definition: DefType = DefType.Str
  }

  def wrapped[T](key: String, asJson: T => Json, fromJson: Json => T, definition: DefType = DefType.Dynamic): RW[T] =
    RW.from(
      r = t => obj(key -> asJson(t)),
      w = j => fromJson(j(key)),
      d = DefType.Obj(key -> definition)
    )

  /**
    * Convenience functionality to provide a static / singleton value that
    * represents that type
    *
    * @param value
    *   the singleton value to use
    */
  def static[T](value: T): RW[T] = from(_ => obj(), _ => value, DefType.Obj())

  /**
    * Convenience functionality for working with enumerations
    *
    * @param fieldName
    *   the field name to refer to in the Json
    * @param mapping
    *   a mapping of key/value pairs representing the String in fieldName to the
    *   representative value
    */
  def enumeration[T](fieldName: String, mapping: (String, T)*): RW[T] = {
    val f2T: Map[String, T] = mapping.toMap
    val t2F: Map[T, String] = f2T.map(_.swap)

    from(
      (t: T) => obj(fieldName -> t2F(t)),
      (v: Json) => f2T(v.asObj.value(fieldName).asStr.value),
      DefType.Dynamic
    )
  }

  /**
    * Convenience functionality for working with polymorphic types
    *
    * @param fieldName
    *   the field name stored in the value (defaults to "type")
    * @param getType
    *   a function to determine the field value from an instance (defaults to
    *   the class name with the first character lowercase - defaultGetType)
    * @param types
    *   a list of tuples with the type names associated with their RW
    */
  def poly[P](fieldName: String = "type", getType: P => String = defaultGetType _)(
    types: (String, RW[_ <: P])*
  ): RW[P] = {
    val typeMap = types.toMap
    from(
      r = (p: P) => {
        val `type` = getType(p)
        typeMap.get(`type`) match {
          case Some(rw) => rw.asInstanceOf[RW[P]].read(p).merge(obj("type" -> `type`))
          case None => throw new RuntimeException(
              s"Type not found [${`type`}] converting from value $p. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      w = (json: Json) => {
        val `type` = json(fieldName).asString
        typeMap.get(`type`) match {
          case Some(rw) => rw.write(json)
          case None => throw new RuntimeException(
              s"Type not found [${`type`}] converting from value $json. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      d = DefType.Poly(types.map { case (key, rw) =>
        val obj = rw.definition.asInstanceOf[DefType.Obj]
        key -> obj
      }.toMap)
    )
  }

  /**
    * Used by poly by default to getType using the class name with the first
    * character lowercase
    */
  private def defaultGetType[P](p: P): String = {
    val name = p.getClass.getSimpleName.replace("$", "")
    s"${name.charAt(0).toLower}${name.substring(1)}"
  }
}
