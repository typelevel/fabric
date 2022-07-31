package fabric.filter

import fabric._

object RemoveNullsFilter extends ValueFilter {
  override def apply(value: Json): Option[Json] = if (value.isNull) {
    None
  } else {
    Some(value)
  }
}

