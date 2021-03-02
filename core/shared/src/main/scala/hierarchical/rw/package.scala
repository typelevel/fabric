package hierarchical

import scala.language.experimental.macros

package object rw extends CompileRW {
  implicit class Convertible[T](value: T) {
    def toValue(implicit reader: Reader[T]): Value = reader.read(value)
  }

  implicit class Asable(value: Value) {
    def as[T](implicit writer: Writer[T]): T = writer.write(value)
  }
}