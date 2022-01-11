package fabric.filter

import fabric.Value

import scala.annotation.tailrec

case class ChainedFilter(filters: List[ValueFilter]) extends ValueFilter {
  override def apply(value: Value): Option[Value] = {
    @tailrec
    def recurse(value: Value, filters: List[ValueFilter]): Option[Value] = if (filters.isEmpty) {
      Some(value)
    } else {
      val f = filters.head
      f(value) match {
        case None => None
        case Some(v) => recurse(v, filters.tail)
      }
    }

    recurse(value, filters)
  }
}

object ChainedFilter {
  def apply(filters: ValueFilter*): ChainedFilter = ChainedFilter(filters.toList)
}