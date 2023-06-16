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

/**
  * RW provides a single class representation of a Reader and Writer for the
  * same type
  */
trait RW[T] extends Reader[T] with Writer[T] {
  def definition: DefType

  def withPreWrite(f: Json => Json): RW[T] = EnhancedRW[T](this, preWrite = List(f))
  def withPostRead(f: (T, Json) => Json): RW[T] = EnhancedRW[T](this, postRead = List(f))
}

object RW extends CompileRW {
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
