package hierarchical.rw

import hierarchical._
import hierarchical.rw._

trait ReadableWritable[T] extends Readable[T] with Writable[T]

object ReadableWritable {
  implicit lazy val intRW: ReadableWritable[Int] = apply[Int](i => num(i.toDouble), _.asNum.value.toInt)
  implicit lazy val stringRW: ReadableWritable[String] = apply[String](str, _.asStr.value)

  def apply[T](r: T => Value, w: Value => T): ReadableWritable[T] = new ReadableWritable[T] {
    override def write(value: Value): T = w(value)

    override def read(t: T): Value = r(t)
  }
}