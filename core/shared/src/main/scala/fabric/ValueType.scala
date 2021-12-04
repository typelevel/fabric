package fabric

/**
 * ValueType represents the possible types of Value
 */
sealed trait ValueType[T] {
  def is(`type`: ValueType[_]): Boolean = this == `type`
}

case object ValueType {
  case object Obj extends ValueType[fabric.Obj]
  case object Arr extends ValueType[fabric.Arr]
  case object Str extends ValueType[fabric.Str]
  case object Num extends ValueType[fabric.Num]
  case object NumInt extends ValueType[fabric.NumInt] {
    override def is(`type`: ValueType[_]): Boolean = super.is(`type`) || `type` == Num
  }
  case object NumDec extends ValueType[fabric.NumDec] {
    override def is(`type`: ValueType[_]): Boolean = super.is(`type`) || `type` == Num
  }
  case object Bool extends ValueType[fabric.Bool]
  case object Null extends ValueType[fabric.Null]
}