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
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RWSpecAuto extends AnyWordSpec with Matchers {
  "automatic conversion" should {
    "convert Person to Json and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.json
      value should be(
        obj(
          "name" -> "Matt Hicks",
          "age" -> 41,
          "address" -> obj(
            "city" -> "San Jose",
            "state" -> "California"
          )
        )
      )
      val back = value.as[Person]
      back should be(person)
    }
    "convert from empty obj to Defaults" in {
      val v = obj()
      val d = v.as[Defaults]
      d.name should be("John Doe")
      d.age should be(21)
    }
    "convert from single argument to Defaults" in {
      val v = obj("name" -> "Jane Doe")
      val d = v.as[Defaults]
      d.name should be("Jane Doe")
      d.age should be(21)
    }
    "supporting generic type on case class" in {
      val w = Wrapper(
        "Test1",
        Address("San Jose", "California"),
        Some(Address("Norman", "Oklahoma"))
      )
      val value = w.json
      value should be(
        obj(
          "name" -> "Test1",
          "value" -> obj(
            "city" -> "San Jose",
            "state" -> "California"
          ),
          "other" -> obj(
            "city" -> "Norman",
            "state" -> "Oklahoma"
          )
        )
      )
      val w2 = value.as[Wrapper[Address]]
      w2 should be(w)
    }
    "supporting Values in conversions" in {
      val w = Wrapper(
        "Test2",
        obj("city" -> "San Jose"),
        Some(obj("city" -> "Norman"))
      )
      val value = w.json
      value should be(
        obj(
          "name" -> "Test2",
          "value" -> obj(
            "city" -> "San Jose"
          ),
          "other" -> obj(
            "city" -> "Norman"
          )
        )
      )
    }
    // TODO: Enable once Scala 3 support for sealed traits is working
//    "supporting sealed traits" in {
//      val car: VehicleType = VehicleType.Car
//      car.json should be(Str("Car"))
//      "SUV".json.as[VehicleType] should be(VehicleType.SUV)
//    }
  }
}
