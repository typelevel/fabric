package hierarchical.rw

import hierarchical._

trait Readable[T] {
  def read(t: T): Value
}

trait Writable[T] {
  def write(value: Value): T
}

trait ReadableWritable[T] extends Readable[T] with Writable[T]

object IntRW extends ReadableWritable[Int] {
  override def read(t: Int): Value = num(t.toDouble)

  override def write(value: Value): Int = value.asNum.value.toInt
}

object StringRW extends ReadableWritable[String] {
  override def read(t: String): Value = str(t)

  override def write(value: Value): String = value.asStr.value
}

trait ClassRW[T] extends ReadableWritable[T] {
  protected def t2Map(t: T): Map[String, Value]
  protected def map2T(map: Map[String, Value]): T

  override def read(t: T): Value = Obj(t2Map(t))

  override def write(value: Value): T = map2T(value.asObj.value)
}