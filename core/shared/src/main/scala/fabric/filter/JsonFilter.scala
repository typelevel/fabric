/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.filter

import fabric.{Arr, Json, JsonPath, Obj}

trait JsonFilter {
  def apply(value: Json, path: JsonPath): Option[Json]

  protected def filters: List[JsonFilter] = List(this)

  def &&(that: JsonFilter): JsonFilter = {
    val left = this.filters
    val right = that.filters
    JsonFilters(left ::: right)
  }
}

object JsonFilter {
  def apply(filter: JsonFilter, json: Json): Option[Json] =
    filter.filters.foldLeft[Option[Json]](Some(json)) { (result, filter) =>
      result.flatMap(json => internal(filter, json, JsonPath.empty))
    }

  private def internal(
    filter: JsonFilter,
    json: Json,
    path: JsonPath
  ): Option[Json] = json match {
    case Obj(map) =>
      val mutated = map
        .map { case (key, value) =>
          internal(filter, value, path \ key).map(updated => key -> updated)
        }
        .flatten
        .toList
      filter(Obj(mutated: _*), path)
    case Arr(vector) =>
      val mutated = vector.zipWithIndex.flatMap { case (value, index) =>
        internal(filter, value, path \ s"[$index]")
      }
      filter(Arr(mutated), path)
    case _ => filter(json, path)
  }
}
