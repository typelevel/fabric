package hierarchical

import scala.language.experimental.macros

package object rw extends CompileRW {
  implicit class Convertible[T](value: T) {
    def toValue(implicit readable: Readable[T]): Value = readable.read(value)
  }

  implicit class Asable(value: Value) {
    def as[T](implicit writable: Writable[T]): T = writable.write(value)
  }
}