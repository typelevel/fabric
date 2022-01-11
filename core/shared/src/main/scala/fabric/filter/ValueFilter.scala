package fabric.filter

import fabric.Value

trait ValueFilter {
  def apply(value: Value): Option[Value]
}

