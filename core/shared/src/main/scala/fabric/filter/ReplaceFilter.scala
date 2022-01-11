package fabric.filter

import fabric.Value

case class ReplaceFilter(find: Value, replacement: Value) extends ValueFilter {
  override def apply(value: Value): Option[Value] = if (value == find) {
    Some(replacement)
  } else {
    Some(value)
  }
}
