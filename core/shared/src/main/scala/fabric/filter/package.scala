package fabric

package object filter {
  implicit class ValueFilterExtras(val value: Value) extends AnyVal {
    def snake2Camel: Value = value.filter(SnakeToCamelFilter).get
    def camel2Snake: Value = value.filter(CamelToSnakeFilter).get
    def withoutNulls: Option[Value] = value.filter(RemoveNullsFilter)
    def withoutEmpty: Option[Value] = value.filter(RemoveEmptyFilter)
    def replace(find: Value, replacement: Value): Value = value.filter(ReplaceFilter(find, replacement)).get
  }
}