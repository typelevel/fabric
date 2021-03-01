package hierarchical

/**
 * ValueType represents the possible types of Value
 */
sealed trait ValueType

case object ValueType {
  case object Obj extends ValueType
  case object Arr extends ValueType
  case object Str extends ValueType
  case object Num extends ValueType
  case object Bool extends ValueType
  case object Null extends ValueType
}