package hierarchical.rw

import hierarchical._

trait Readable[T] {
  def read(t: T): Value
}

object Readable {
  import ReadableWritable._

  implicit def intR: Readable[Int] = intRW
  implicit def stringR: Readable[String] = stringRW
  implicit def listR[T](implicit r: Readable[T]): Readable[List[T]] = apply[List[T]] { list =>
    Arr(list.map(r.read).toVector)
  }
  implicit def optionR[T](implicit r: Readable[T]): Readable[Option[T]] = apply[Option[T]] {
    case Some(t) => r.read(t)
    case None => Null
  }

  def apply[T](f: T => Value): Readable[T] = new Readable[T] {
    override def read(t: T): Value = f(t)
  }
}