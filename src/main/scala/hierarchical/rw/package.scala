package hierarchical

package object rw {
  implicit def intRW: ReadableWritable[Int] = IntRW
  implicit def stringRW: ReadableWritable[String] = StringRW
}