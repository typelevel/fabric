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

package fabric

import scala.util.matching.Regex

package object search {
  implicit def string2Search(name: String): SearchEntry = ByName(name)
  implicit def int2Search(index: Int): SearchEntry =
    ByOffset(index, OffsetDirection.FromTop)
  implicit def regex2Search(regex: Regex): SearchEntry = ByRegex(regex)

  implicit class JsonSearchExtras(val json: Json) extends AnyVal {
    def search(entries: SearchEntry*): List[JsonPath] =
      SearchEntry.search(json, entries.toList, JsonPath.empty)
  }

  def * : Wildcard.type = Wildcard
  def ** : DoubleWildcard.type = DoubleWildcard
  val first: SearchEntry = ByOffset(0, OffsetDirection.FromTop)
  val last: SearchEntry = ByOffset(0, OffsetDirection.FromBottom)
  def nth(index: Int): SearchEntry = ByOffset(index, OffsetDirection.FromTop)
  def nthFromBottom(index: Int): SearchEntry =
    ByOffset(index, OffsetDirection.FromBottom)
}
