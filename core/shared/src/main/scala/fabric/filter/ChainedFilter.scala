package fabric.filter

import fabric.Json

import scala.annotation.tailrec

case class ChainedFilter(filters: List[ValueFilter]) extends ValueFilter {
  override def apply(value: Json): Option[Json] = {
    @tailrec
    def recurse(value: Json, filters: List[ValueFilter]): Option[Json] = if (filters.isEmpty) {
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