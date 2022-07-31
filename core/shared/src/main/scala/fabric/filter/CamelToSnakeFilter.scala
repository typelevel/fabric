package fabric.filter

import fabric.{Obj, Json}

/**
 * Converts camelCase to snake_case in obj keys
 */
object CamelToSnakeFilter extends ValueFilter {
  override def apply(value: Json): Option[Json] = value match {
    case Obj(map) => Some(Obj(map.map {
      case (key, value) => toSnake(key) -> value
    }))
    case _ => Some(value)
  }

  def toSnake(key: String): String = key.flatMap {
    case c if c.isUpper => s"_${c.toLower}"
    case c => c.toString
  }
}