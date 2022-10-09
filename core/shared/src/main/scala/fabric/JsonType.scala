package fabric

/**
 * JsonType represents the possible types of Json
 */
sealed trait JsonType[T] {
  def is(`type`: JsonType[_]): Boolean = this == `type`
}

case object JsonType {
  case object Obj extends JsonType[fabric.Obj]
  case object Arr extends JsonType[fabric.Arr]
  case object Str extends JsonType[fabric.Str]
  case object Num extends JsonType[fabric.Num]
  case object NumInt extends JsonType[fabric.NumInt] {
    override def is(`type`: JsonType[_]): Boolean = super.is(`type`) || `type` == Num
  }
  case object NumDec extends JsonType[fabric.NumDec] {
    override def is(`type`: JsonType[_]): Boolean = super.is(`type`) || `type` == Num
  }
  case object Bool extends JsonType[fabric.Bool]
  case object Null extends JsonType[fabric.Null]
}