package fabric.filter

import fabric._

/**
 * Converts snake_case to camelCase in obj keys
 */
object SnakeToCamelFilter extends ValueFilter {
  override def apply(value: Value): Option[Value] = value match {
    case Obj(map) => Some(Obj(map.map {
      case (key, value) => toCamel(key) -> value
    }))
    case _ => Some(value)
  }

  def toCamel(key: String): String = key
    .toList
    .foldLeft(List.empty[Char]) {
      case ('_' :: xs, c) => c.toUpper :: xs
      case (xs, c) => c :: xs
    }.reverse.mkString
}