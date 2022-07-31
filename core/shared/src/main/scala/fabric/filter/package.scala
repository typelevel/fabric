package fabric

package object filter {
  implicit class ValueFilterExtras(val value: Json) extends AnyVal {
    def snake2Camel: Json = value.filter(SnakeToCamelFilter).get
    def camel2Snake: Json = value.filter(CamelToSnakeFilter).get
    def withoutNulls: Option[Json] = value.filter(RemoveNullsFilter)
    def withoutEmpty: Option[Json] = value.filter(RemoveEmptyFilter)
    def replace(find: Json, replacement: Json): Json = value.filter(ReplaceFilter(find, replacement)).get
  }
}