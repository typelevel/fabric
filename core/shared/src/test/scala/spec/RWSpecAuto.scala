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
import fabric.define.DefType
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RWSpecAuto extends AnyWordSpec with Matchers {
  "automatic conversion" should {
    "convert Person to Json and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.json
      val expected = obj(
        "name" -> "Matt Hicks",
        "age" -> 41,
        "address" -> obj("city" -> "San Jose", "state" -> "California")
      )
      value should be(expected)
      val back = value.as[Person]
      back should be(person)
    }
    "verify the class name of Person" in {
      Person.rw.definition.className should be(Some("spec.Person"))
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
          "value" -> obj("city" -> "San Jose", "state" -> "California"),
          "other" -> obj("city" -> "Norman", "state" -> "Oklahoma")
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
          "value" -> obj("city" -> "San Jose"),
          "other" -> obj("city" -> "Norman")
        )
      )
    }
    "verify Person's DefType" in {
      Person.rw.definition should be(
        DefType.Obj(
          Some("spec.Person"),
          "name" -> DefType.Str,
          "age" -> DefType.Int,
          "address" -> DefType.Obj(Some("spec.Address"), "city" -> DefType.Str, "state" -> DefType.Str)
        )
      )
    }
    "verify multi reader support works" in {
      val user = User("John Doe", _id = "user/1234")
      val json = user.json
      json should be(
        obj("name" -> "John Doe", "_id" -> "user/1234", "num" -> 1234)
      )
    }
    "validate loading of JsonWrapper from Json" in {
      val json = obj("color" -> "Red", "size" -> 5.4, "quantity" -> 10)
      val sample = json.as[WrapperSample]
      sample.color should be("Red")
      sample.size should be(5.4)
      sample.json should be(json)
    }
    "validate persistence of JsonWrapper to Json" in {
      val sample = WrapperSample(
        color = "Green",
        size = 9.2,
        json = obj("quantity" -> 15, "color" -> "Blue")
      )
      val json: Json = sample.asJson
      json should be(obj("color" -> "Green", "size" -> 9.2, "quantity" -> 15).withReference(sample))
    }
    "verify persisting null String values works" in {
      val user = User(null, "abc/123")
      user.json should be(
        obj(
          "name" -> Null,
          "_id" -> "abc/123",
          "num" -> 123
        )
      )
    }
    "supporting sealed traits" in {
      val car: VehicleType = VehicleType.Car
      car.json should be(Str("Car"))
      "SUV".json.as[VehicleType] should be(VehicleType.SUV)
      VehicleType.rw.definition.asInstanceOf[DefType.Enum].values should be(
        List[Json](
          "Car",
          "SUV",
          "Truck",
          "Mini Van"
        )
      )
    }
    "work properly with nulls and defaults" in {
      val json = obj(
        "offset" -> Null,
        "limit" -> Null,
        "list" -> Null
      )
      val test = json.as[DefaultTest]
      test.offset should be(0)
      test.limit should be(100)
      test.list should be(List("Default"))
    }
  }

  case class User(name: String, _id: String) {
    lazy val num: Int = _id.substring(_id.lastIndexOf('/') + 1).toInt
  }

  object User {
    implicit val rw: RW[User] = RW.gen[User].withPostRead { case (u, json) => json.merge(obj("num" -> u.num)) }
  }

  case class WrapperSample(color: String, size: Double, json: Json) extends JsonWrapper

  object WrapperSample {
    implicit val rw: RW[WrapperSample] = RW.gen
  }

  case class DefaultTest(offset: Int = 0, limit: Int = 100, list: List[String] = List("Default"))

  object DefaultTest {
    implicit val rw: RW[DefaultTest] = RW.gen
  }
}
