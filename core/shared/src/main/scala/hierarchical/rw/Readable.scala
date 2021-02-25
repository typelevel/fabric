package hierarchical.rw

import hierarchical._

trait Readable[T] {
  def read(t: T): Value
}