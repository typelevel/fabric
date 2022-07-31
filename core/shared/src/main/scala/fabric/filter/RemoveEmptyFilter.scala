package fabric.filter

import fabric.{Arr, Obj, Json}

object RemoveEmptyFilter extends ValueFilter {
  override def apply(value: Json): Option[Json] = value match {
    case Obj(map) if map.isEmpty => None
    case Arr(vector) if vector.isEmpty => None
    case _ => Some(value)
  }
}