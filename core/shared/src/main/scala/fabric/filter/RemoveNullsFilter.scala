package fabric.filter

import fabric._

object RemoveNullsFilter extends ValueFilter {
  override def apply(value: Value): Option[Value] = if (value.isNull) {
    None
  } else {
    Some(value)
  }
}
