package fabric.search

import fabric.{Arr, Json, JsonPath, Obj}

case object DoubleWildcard extends SearchEntry {
  override def search(
    json: Json,
    entries: List[SearchEntry],
    jsonPath: JsonPath
  ): List[JsonPath] = searchInternal(
    json = json,
    entries = entries,
    jsonPath = jsonPath,
    matched = false
  )

  protected def searchInternal(
    json: Json,
    entries: List[SearchEntry],
    jsonPath: JsonPath,
    matched: Boolean
  ): List[JsonPath] = {
    val results = if (matched) {
      entries.head.search(json, entries.tail, jsonPath)
    } else {
      Nil
    }
    if (results.nonEmpty) {
      results
    } else {
      json match {
        case Obj(map) =>
          map.toList.flatMap { case (key, value) =>
            searchInternal(value, entries, jsonPath \ key, matched = true)
          }
        case Arr(vec) =>
          vec.toList.zipWithIndex.flatMap { case (value, index) =>
            searchInternal(value, entries, jsonPath \ index, matched = true)
          }
        case _ => Nil
      }
    }
  }
}