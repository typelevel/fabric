package hierarchical.rw

import hierarchical.Value

trait Writable[T] {
  def write(value: Value): T
}