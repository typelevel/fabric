package fabric.filter

import fabric.Json

object NoOpFilter extends ValueFilter {
  override def apply(value: Json): Option[Json] = Some(value)
}