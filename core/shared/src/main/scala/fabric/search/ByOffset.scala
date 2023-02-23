package fabric.search

import fabric._

import scala.util.Try

case class ByOffset(offset: Int, direction: OffsetDirection) extends SearchEntry {
  override def search(json: Json, entries: List[SearchEntry], jsonPath: JsonPath): List[JsonPath] = json match {
    case Arr(vec) =>
      val index = direction match {
        case OffsetDirection.FromTop => offset
        case OffsetDirection.FromBottom => vec.length - 1 - offset
      }
      Try(vec(index)).toOption match {
        case Some(value) => SearchEntry.search(value, entries, jsonPath \ index)
        case None => Nil
      }
    case _ => Nil
  }
}