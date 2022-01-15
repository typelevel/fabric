package fabric.filter

import fabric.Value

object NoOpFilter extends ValueFilter {
  override def apply(value: Value): Option[Value] = Some(value)
}