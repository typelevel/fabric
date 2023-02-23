package fabric.search

import fabric.{Json, JsonPath, Obj}

import scala.util.matching.Regex

case class ByRegex(regex: Regex) extends AnyVal with SearchEntry {
  override def search(
    json: Json,
    entries: List[SearchEntry],
    jsonPath: JsonPath
  ): List[JsonPath] = json match {
    case Obj(map) =>
      map.toList.filter {
        case (key, _) => regex.matches(key)
      }.flatMap {
        case (key, value) => SearchEntry.search(value, entries, jsonPath \ key)
      }
    case _ => Nil
  }
}