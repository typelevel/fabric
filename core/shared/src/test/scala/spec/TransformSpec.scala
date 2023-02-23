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
import fabric.filter.RemoveEmptyFilter
import fabric.transform._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TransformSpec extends AnyWordSpec with Matchers {
  "Transform" should {
    "copy a simple object" in {
      val json = obj(
        "level1" -> obj(
          "level2" -> obj(
            "product" -> obj("name" -> "Product Name", "sku" -> 12345)
          )
        )
      )
      val transformed = json
        .transform("level1", "level2", "product")
        .copy("product")
      transformed should be(
        obj(
          "level1" -> obj(
            "level2" -> obj(
              "product" -> obj("name" -> "Product Name", "sku" -> 12345)
            )
          ),
          "product" -> obj("name" -> "Product Name", "sku" -> 12345)
        )
      )
    }
    "move a simple object" in {
      val json = obj(
        "level1" -> obj(
          "level2" -> obj(
            "product" -> obj("name" -> "Product Name", "sku" -> 12345)
          )
        )
      )
      val transformed = json
        .transform("level1", "level2", "product")
        .move()
        .filterOne(RemoveEmptyFilter)
      transformed should be(obj("name" -> "Product Name", "sku" -> 12345))
    }
  }
}
