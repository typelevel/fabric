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
import fabric.search._
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
      val transformed = json.transform(Search("level1", "level2", "product")).copy("product")
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
      val transformed = json.transform(Search("level1", "level2", "product")).move().filterOne(RemoveEmptyFilter)
      transformed should be(obj("name" -> "Product Name", "sku" -> 12345))
    }
    "rename a top-level entry" in {
      val json = obj(
        "name" -> "John Doe"
      )
      val transformed = json.transform(Search("name")).rename("fullName")
      transformed should be(
        obj(
          "fullName" -> "John Doe"
        )
      )
    }
    "rename a simple object" in {
      val json = obj(
        "product" -> List(
          obj("entryName" -> "Apple"),
          obj("entryName" -> "Banana"),
          obj("entryName" -> "Cherry")
        )
      )
      val transformed = json.transform(Search("product", *, "entryName")).rename("name")
      transformed should be(
        obj(
          "product" -> List(
            obj("name" -> "Apple"),
            obj("name" -> "Banana"),
            obj("name" -> "Cherry")
          )
        )
      )
    }
    "merge a few records" in {
      val json = obj(
        "one" -> obj(
          "product" -> obj(
            "name" -> "Slinky"
          )
        ),
        "two" -> obj(
          "product" -> obj(
            "type" -> "Toy"
          )
        ),
        "three" -> obj(
          "product" -> obj(
            "price" -> 50.0
          )
        ),
        "product" -> obj()
      )
      val transformed = json.transform(Search(*, "product")).mergeTo("product")
      transformed should be(
        obj(
          "one" -> obj(
            "product" -> obj(
              "name" -> "Slinky"
            )
          ),
          "two" -> obj(
            "product" -> obj(
              "type" -> "Toy"
            )
          ),
          "three" -> obj(
            "product" -> obj(
              "price" -> 50.0
            )
          ),
          "product" -> obj(
            "name" -> "Slinky",
            "type" -> "Toy",
            "price" -> 50.0
          )
        )
      )
    }
    "concatenate multiple strings" in {
      val json = obj(
        "first" -> "This",
        "second" -> "is",
        "third" -> "multiple",
        "fourth" -> "strings"
      )
      val transformed = json.transform(Search(*)).concatenate("merged", " ")
      transformed should be(
        obj(
          "first" -> "This",
          "second" -> "is",
          "third" -> "multiple",
          "fourth" -> "strings",
          "merged" -> "This is multiple strings"
        )
      )
    }
    "extract from simple regex" in {
      val json = obj("time" -> "12:34:56")
      val transformed = json
        .transform(Search("time"))
        .extract(
          regex = "(.+):(.+):(.+)".r,
          to = Vector(
            "hour",
            "minute",
            "second"
          )
        )
      transformed should be(
        obj(
          "time" -> "12:34:56",
          "hour" -> "12",
          "minute" -> "34",
          "second" -> "56"
        )
      )
    }
    "delete a simple object" in {
      val json = obj(
        "first" -> obj(
          "second" -> 2,
          "third" -> 3
        )
      )
      val transformed = json.transform(Search("first", "second")).delete().filterOne(RemoveEmptyFilter)
      transformed should be(
        obj(
          "first" -> obj(
            "third" -> 3
          )
        )
      )
    }
  }
}
