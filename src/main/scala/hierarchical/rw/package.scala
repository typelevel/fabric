package hierarchical

package object rw {
  implicit def intRW: ReadableWritable[Int] = IntRW
  implicit def stringRW: ReadableWritable[String] = StringRW

  implicit class Convertible[T](value: T) {
    def toValue(implicit readable: Readable[T]): Value = readable.read(value)
  }

  implicit class Asable(value: Value) {
    def as[T](implicit writable: Writable[T]): T = writable.write(value)
  }
}