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

import fabric.*
import fabric.cryo.{ByteBufferPool, Cryo}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CryoSpec extends AnyWordSpec with Matchers {
  private val largeJson = obj(
    "name" -> "Matt \"Matteo\" Hicks",
    "age" -> 41,
    "numbers" -> List(1, 2, 3),
    "address" -> obj(
      "street" -> "123 Somewhere Rd.\nBox 123",
      "city" -> "San Jose",
      "state" -> "California",
      "zipcode" -> 95136
    )
  )

  "Cryo" should {
    def freezeAndThaw(json: Json): Assertion = {
      val bb = Cryo.freeze(json, allocateDirect = true)
      bb.flip()
      val thawed = Cryo.thaw(bb)
      thawed should be(json)
    }
    "freeze and thaw a Str properly" in {
      val json = Str("Hello, World!")
      freezeAndThaw(json)
    }
    "freeze and thaw a simple obj" in {
      val json = obj("value" -> 5)
      freezeAndThaw(json)
    }
    "freeze and thaw a complex obj" in {
      freezeAndThaw(largeJson)
    }
    "freeze and thaw a simple obj using the pool" in {
      val json = obj("value" -> 5)
      val bytes = Cryo.freeze(json)
      val thawed = Cryo.thaw(bytes)
      json should be(thawed)
    }
    "freeze and that a multi-type obj using the pool" in {
      val json = obj(
        "string" -> "Hello, World",
        "integer" -> 42,
        "bigDecimal" -> BigDecimal(123),
        "array" -> arr(
          1,
          "two",
          BigDecimal(3)
        ),
        "bool" -> true,
        "null" -> Null
      )
      val expectedSize = Cryo.bytes(json)
      val bytes = Cryo.freeze(json)
      bytes.length should be(expectedSize)
      val thawed = Cryo.thaw(bytes)
      json should be(thawed)
    }
    "freeze and thaw null" in {
      Cryo.thaw(Cryo.freeze(Null)) should be(Null)
    }
    "dispose the pool and overflow the new pool to verify resizing" in {
      ByteBufferPool.dispose()
      ByteBufferPool.ByteBufferSize = 10
      val bytes = Cryo.freeze(largeJson)
      val thawed = Cryo.thaw(bytes)
      largeJson should be(thawed)
      ByteBufferPool.ByteBufferSize should be(320)
    }
  }
}
