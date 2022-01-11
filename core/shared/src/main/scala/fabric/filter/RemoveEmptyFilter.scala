package fabric.filter

import fabric.{Arr, Obj, Value}

object RemoveEmptyFilter extends ValueFilter {
  override def apply(value: Value): Option[Value] = value match {
    case Obj(map) if map.isEmpty => None
    case Arr(vector) if vector.isEmpty => None
    case _ => Some(value)
  }
}