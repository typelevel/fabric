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
import fabric.filter._
import fabric.merge.{ArrConcatMerge, MergeConfig}
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FabricSpec extends AnyWordSpec with Matchers {
  val v: Obj = obj(
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

  "Fabric" should {
    "represent AST properly" in {
      v should be(
        obj(
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
      )
    }
    "verify type getting works as expected" in {
      val s: fabric.Json = Str("Hello, World!")
      s.getAsType(JsonType.Str) should be(Some(Str("Hello, World!")))
      s.getStr should be(Some(Str("Hello, World!")))
      s.getAsType(JsonType.Obj) should be(None)
      s.getAsType(JsonType.Bool) should be(None)
      s.getAsType(JsonType.Arr) should be(None)
      s.getAsType(JsonType.Num) should be(None)
      s.getAsType(JsonType.Null) should be(None)
    }
    "extract the state" in {
      val state = v("address" \ "state")
      state should be(str("California"))
    }
    "update the hierarchy" in {
      val updated = v.modify("address" \ "state") { value =>
        str("Tennessee")
      }
      updated should be(
        obj(
          "name" -> "Matt \"Matteo\" Hicks",
          "age" -> 41,
          "numbers" -> List(1, 2, 3),
          "address" -> obj(
            "street" -> "123 Somewhere Rd.\nBox 123",
            "city" -> "San Jose",
            "state" -> "Tennessee",
            "zipcode" -> 95136
          )
        )
      )
    }
    "remove from the hierarchy" in {
      val removed = v.remove("address" \ "state")
      removed should be(
        obj(
          "name" -> "Matt \"Matteo\" Hicks",
          "age" -> 41,
          "numbers" -> List(1, 2, 3),
          "address" -> obj(
            "street" -> "123 Somewhere Rd.\nBox 123",
            "city" -> "San Jose",
            "zipcode" -> 95136
          )
        )
      )
    }
    "properly merge a simple scenario" in {
      val v1 = obj(
        "name" -> "Matt Hicks",
        "age" -> 41,
        "numbers" -> List(1, 2, 3),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.",
          "city" -> "San Jose"
        )
      )
      val v2 = obj(
        "age" -> 42,
        "numbers" -> List(4, 5, 6),
        "address" -> obj(
          "state" -> "California"
        )
      )
      val merged = v1.merge(v2)
      val expected = obj(
        "name" -> "Matt Hicks",
        "age" -> 42,
        "numbers" -> List(4, 5, 6),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.",
          "city" -> "San Jose",
          "state" -> "California"
        )
      )
      merged should be(expected)
    }
    "convert to/from Special" in {
      val s = obj().as[Special]
      s.name should be(None)
      s.age should be(21)
      s.data should be(None)
    }
    "use polymorphic values" in {
      val json1 = obj("type" -> "blank")
      val json2 = obj("type" -> "polyValue", "s" -> "Hello, World!")

      val p1 = json1.as[Polymorphic]
      p1 should be(Polymorphic.Blank)
      p1.json should be(json1)

      val p2 = json2.as[Polymorphic]
      p2 should be(Polymorphic.PolyValue("Hello, World!"))
      p2.json should be(json2)
    }
    "include or exclude null fields" in {
      val json1 = obj(
        "one" -> Null,
        "two" -> 2,
        "three" -> "three"
      )
      Obj.ExcludeNullValues = true
      val json2 = obj(
        "one" -> Null,
        "two" -> 2,
        "three" -> "three"
      )
      Obj.ExcludeNullValues = false
      json1 should be(
        obj(
          "one" -> Null,
          "two" -> 2,
          "three" -> "three"
        )
      )
      json2 should be(
        obj(
          "two" -> 2,
          "three" -> "three"
        )
      )
    }
    "convert snake-case to camel-case" in {
      val snake = obj(
        "first_level" -> obj(
          "second_level" -> obj(
            "third_level_and_last" -> "Test"
          )
        )
      )
      val camel = obj(
        "firstLevel" -> obj(
          "secondLevel" -> obj(
            "thirdLevelAndLast" -> "Test"
          )
        )
      )
      val snake2Camel = snake.snake2Camel
      snake2Camel should be(camel)

      val camel2Snake = camel.camel2Snake
      camel2Snake should be(snake)
    }
    "merge with a custom override" in {
      val json1 = obj(
        "test1" -> obj(
          "test2" -> arr(1, 2, 3),
          "test3" -> arr(1, 2, 3)
        )
      )
      val json2 = obj(
        "test1" -> obj(
          "test2" -> arr(4, 5, 6),
          "test3" -> arr(4, 5, 6)
        )
      )
      val merged = json1.merge(
        json2,
        config =
          MergeConfig.withOverride(Path.parse("test1.test2"), ArrConcatMerge)
      )
      merged should be(
        obj(
          "test1" -> obj(
            "test2" -> arr(1, 2, 3, 4, 5, 6),
            "test3" -> arr(4, 5, 6)
          )
        )
      )
    }
  }
}

case class Special(name: Option[String], age: Int = 21, data: Option[Json])

object Special {
  implicit val rw: RW[Special] = RW.gen
}

sealed trait Polymorphic

object Polymorphic {
  implicit val rw: RW[Polymorphic] = RW.poly[Polymorphic]() {
    case "blank" => RW.static(Blank)
    case "polyValue" => PolyValue.rw
  }

  case object Blank extends Polymorphic

  case class PolyValue(s: String) extends Polymorphic

  object PolyValue {
    implicit val rw: RW[PolyValue] = RW.gen
  }
}
