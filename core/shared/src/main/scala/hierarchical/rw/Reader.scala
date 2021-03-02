package hierarchical.rw

import hierarchical._

/**
 * Reader provides a simple T => Value wrapper functionality
 */
trait Reader[T] {
  def read(t: T): Value
}

object Reader {
  import ReaderWriter._

  implicit def intR: Reader[Int] = intRW
  implicit def stringR: Reader[String] = stringRW
  implicit def listR[T](implicit r: Reader[T]): Reader[List[T]] = apply[List[T]] { list =>
    Arr(list.map(r.read).toVector)
  }
  implicit def optionR[T](implicit r: Reader[T]): Reader[Option[T]] = apply[Option[T]] {
    case Some(t) => r.read(t)
    case None => Null
  }

  def apply[T](f: T => Value): Reader[T] = new Reader[T] {
    override def read(t: T): Value = f(t)
  }
}