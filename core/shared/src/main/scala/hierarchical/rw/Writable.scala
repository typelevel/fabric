package hierarchical.rw

import hierarchical._

trait Writable[T] {
  def write(value: Value): T
}

object Writable {
  import ReadableWritable._

  implicit def intW: Writable[Int] = intRW
  implicit def stringW: Writable[String] = stringRW
  implicit def listW[T](implicit w: Writable[T]): Writable[List[T]] = apply[List[T]] {
    case Arr(vector) => vector.toList.map(w.write)
    case v => throw new RuntimeException(s"Unsupported list: $v")
  }
  implicit def optionW[T](implicit w: Writable[T]): Writable[Option[T]] = apply[Option[T]] {
    case Null => None
    case v => Option(w.write(v))
  }

  def apply[T](f: Value => T): Writable[T] = new Writable[T] {
    override def write(value: Value): T = f(value)
  }
}