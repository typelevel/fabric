package fabric.filter

import fabric.Json

case class ReplaceFilter(find: Json, replacement: Json) extends ValueFilter {
  override def apply(value: Json): Option[Json] = if (value == find) {
    Some(replacement)
  } else {
    Some(value)
  }
}
