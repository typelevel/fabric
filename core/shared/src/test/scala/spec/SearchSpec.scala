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

package spec

import fabric._
import fabric.dsl.*
import fabric.search._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SearchSpec extends AnyWordSpec with Matchers {
  "Search" should {
    "find a single path" in {
      val json = obj("one" -> obj("two" -> obj("three" -> 3)))
      val results = json.search("one", "two", "three")
      results should be(List(JsonPath("one", "two", "three")))
    }
    "find multiple with a wildcard" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> "last")
        )
      )
      val results = json.search("one", *, "end")
      results should be(
        List(JsonPath("one", "two", "end"), JsonPath("one", "three", "end"))
      )
    }
    "find several with multiple wildcards" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> "last"),
          "four" -> obj("final" -> "done")
        )
      )
      val results = json.search(*, *, *)
      results should be(
        List[JsonPath](
          JsonPath("one", "two", "end"),
          JsonPath("one", "three", "end"),
          JsonPath("one", "four", "final")
        )
      )
    }
    "find one with multiple wildcards and late filtering" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> "last"),
          "four" -> obj("final" -> "done")
        )
      )
      val results = json.search(*, *, "final")
      results should be(List(JsonPath("one", "four", "final")))
    }
    "find one double wildcard" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> "last"),
          "four" -> obj("final" -> "done")
        )
      )
      val results = json.search(**, "final")
      results should be(List(JsonPath("one", "four", "final")))
    }
    "find two double wildcard" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> obj("not-yet" -> obj("final" -> "super!"))),
          "four" -> obj("final" -> "done")
        )
      )
      val results = json.search(**, "final")
      results should be(
        List[JsonPath](
          JsonPath("one", "three", "end", "not-yet", "final"),
          JsonPath("one", "four", "final")
        )
      )
    }
    "find via regex" in {
      val json = obj(
        "one" -> obj(
          "two" -> obj("end" -> "last"),
          "three" -> obj("end" -> obj("not-yet" -> obj("final" -> "super!"))),
          "four" -> obj("final" -> "done")
        )
      )
      val results = json.search(**, "t.+".r)
      results should be(List(JsonPath("one", "two"), JsonPath("one", "three")))
    }
    "find via nth" in {
      val json = obj("list" -> arr(obj("one" -> 1), obj("two" -> 2), obj("three" -> 3)))

      val results = json.search("list", nth(1))
      results should be(List(JsonPath("list", 1)))
    }
    "find via last" in {
      val json = obj("list" -> arr(obj("one" -> 1), obj("two" -> 2), obj("three" -> 3)))

      val results = json.search("list", last)
      results should be(List(JsonPath("list", 2)))
    }
  }
}
