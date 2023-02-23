package fabric.search

sealed trait OffsetDirection

object OffsetDirection {
  case object FromTop extends OffsetDirection
  case object FromBottom extends OffsetDirection
}
