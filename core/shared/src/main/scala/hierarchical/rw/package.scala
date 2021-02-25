package hierarchical

import scala.language.experimental.macros

package object rw extends CompileRW {
  implicit lazy val intRW: ReadableWritable[Int] = rw[Int](i => num(i.toDouble), _.asNum.value.toInt)
  implicit lazy val stringRW: ReadableWritable[String] = rw[String](str, _.asStr.value)

  implicit class Convertible[T](value: T) {
    def toValue(implicit readable: Readable[T]): Value = readable.read(value)
  }

  implicit class Asable(value: Value) {
    def as[T](implicit writable: Writable[T]): T = writable.write(value)
  }

  def rw[T](r: T => Value, w: Value => T): ReadableWritable[T] = new ReadableWritable[T] {
    override def write(value: Value): T = w(value)

    override def read(t: T): Value = r(t)
  }
}