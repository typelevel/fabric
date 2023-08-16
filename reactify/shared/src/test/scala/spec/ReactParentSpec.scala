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
import fabric.react.ReactParent
import reactify._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReactParentSpec extends AnyWordSpec with Matchers {
  "ReactParent" should {
    "properly handle initialization of Person" in {
      Person.name() should be("John Doe")
      Person.age() should be(21)
      Person.json() should be(
        obj(
          "name" -> "John Doe",
          "age" -> 21
        )
      )
    }
    "change the name and age" in {
      Person.name @= "Jane Doe"
      Person.age @= 20
      Person.json() should be(
        obj(
          "name" -> "Jane Doe",
          "age" -> 20
        )
      )
    }
  }
}

object Person extends ReactParent {
  val name: Var[String] = reactive[String]("name", "")
  val age: Var[Int] = reactive[Int]("age", 0)

  override protected def load(): Option[Json] = Some(
    obj(
      "name" -> "John Doe",
      "age" -> 21
    )
  )
}
