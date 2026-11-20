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
import fabric.dsl._
import fabric.io._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.scalajs.js

// An engine without BigInt (Chromium 38, webOS 3): the parser must not name the BigInt global, which there throws a
// ReferenceError for every value that reaches the check
class NoBigIntSpec extends AnyWordSpec with Matchers {
  private def withoutBigInt[T](f: => T): T = {
    val global = js.Dynamic.global.globalThis
    val saved = global.BigInt
    js.special.delete(global, "BigInt")
    try f
    finally global.BigInt = saved
  }

  "Parsing without a BigInt global" should {
    "parse objects, arrays, numbers, strings and nested values" in {
      val json = withoutBigInt(JsonParser("""{"a":1,"b":[true,null,"x"],"c":{"d":-2.5,"e":"y"},"f":[[1,2],{}]}"""))
      json should be(
        obj(
          "a" -> 1,
          "b" -> arr(true, Null, "x"),
          "c" -> obj("d" -> -2.5, "e" -> "y"),
          "f" -> arr(arr(1, 2), obj())
        )
      )
    }
    "parse an object holding a number beyond a Double" in {
      withoutBigInt(JsonParser("""{"big":9007199254740993}""")) should be(obj("big" -> 9007199254740993L))
    }
    "write and read back an object" in {
      val json = obj("a" -> 1, "b" -> arr(true, Null, "x"), "c" -> obj("s" -> "q\"uote"))
      withoutBigInt(JsonParser(JsonFormatter.Compact(json))) should be(json)
    }
  }
}
