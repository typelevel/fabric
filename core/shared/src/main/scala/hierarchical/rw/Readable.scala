package hierarchical.rw

import hierarchical._

trait Readable[T] {
  def read(t: T): Value
}

object Readable {
  import ReadableWritable._

  implicit def intR: Readable[Int] = intRW
  implicit def stringR: Readable[String] = stringRW
}