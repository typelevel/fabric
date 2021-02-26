package hierarchical.rw

import hierarchical.Value

trait Writable[T] {
  import ReadableWritable._

  implicit def intW: Writable[Int] = intRW
  implicit def stringW: Writable[String] = stringRW

  def write(value: Value): T
}