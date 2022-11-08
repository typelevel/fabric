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

import fabric.io.SimpleJsonParser
import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SimpleJsonParserSpec extends AnyWordSpec with Matchers {
  "SimpleJsonParser" should {
    "parse a simple String" in {
      val json = SimpleJsonParser(""""name"""")
      json should be(str("name"))
    }
    "parse a simple integer" in {
      val json = SimpleJsonParser("215")
      json should be(num(215))
    }
    "parse a simple decimal" in {
      val json = SimpleJsonParser("2.15")
      json should be(num(2.15))
    }
    "parse a simple array" in {
      val json = SimpleJsonParser("[1, 2, 3]")
      json should be(arr(1, 2, 3))
    }
    "parse a simple object" in {
      val json = SimpleJsonParser("""{"name": "Testing"}""")
      json should be(obj("name" -> "Testing"))
    }
    "parse a slightly more complex object" in {
      val json = SimpleJsonParser("""{"name": "Matt Hicks", "age": 41}""")
      json should be(
        obj(
          "name" -> "Matt Hicks",
          "age" -> 41
        )
      )
    }
  }
}
