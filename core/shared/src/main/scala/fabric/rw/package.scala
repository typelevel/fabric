package fabric

import scala.language.experimental.macros

package object rw extends CompileRW {
  type RW[T] = ReaderWriter[T]

  implicit class Convertible[T](value: T) {
    def json(implicit reader: Reader[T]): Json = reader.read(value)
  }

  implicit class Asable(value: Json) {
    def as[T](implicit writer: Writer[T]): T = writer.write(value)
  }

  /**
   * Convenience functionality to provide a static / singleton value that represents that type
   *
   * @param value the singleton value to use
   */
  def staticRW[T](value: T): ReaderWriter[T] = ReaderWriter(_ => obj(), _ => value)

  /**
   * Convenience functionality for working with enumerations
   *
   * @param fieldName the field name to refer to in the Value
   * @param mapping a mapping of key/value pairs representing the String in fieldName to the representative value
   */
  def enumRW[T](fieldName: String, mapping: (String, T)*): ReaderWriter[T] = {
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
   * @param getType a function to determine the field value from an instance (defaults to the class name with the first character lowercase - defaultGetType)
   * @param matcher a matcher for field values to get the representative ReaderWriter for that type
   */
  def polyRW[P](fieldName: String = "type",
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
   * Used by polyRW by default to getType using the class name with the first character lowercase
   */
  def defaultGetType[P](p: P): String = {
    val name = p.getClass.getSimpleName.replace("$", "")
    s"${name.charAt(0).toLower}${name.substring(1)}"
  }
}