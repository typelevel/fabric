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

import scala.collection.immutable.VectorMap

class RWSpecManual extends AnyWordSpec with Matchers {
  implicit val addressRW: RW[Address] = new ClassRW[Address] {
    override protected def t2Map(t: Address): Map[String, Json] =
      VectorMap("city" -> t.city.json, "state" -> t.state.json)

    override protected def map2T(map: Map[String, Json]): Address =
      Address(city = map("city").as[String], state = map("state").as[String])

    override def definition: DefType = DefType.Obj("city" -> DefType.Str, "state" -> DefType.Str)
  }
  implicit val personRW: RW[Person] = new ClassRW[Person] {
    override protected def t2Map(t: Person): Map[String, Json] = VectorMap(
      "name" -> t.name.json,
      "age" -> t.age.json,
      "address" -> t.address.json
    )

    override protected def map2T(map: Map[String, Json]): Person = Person(
      name = map("name").as[String],
      age = map("age").as[Int],
      address = map("address").as[Address]
    )

    override def definition: DefType = DefType.Obj(
      "name" -> DefType.Str,
      "age" -> DefType.Int,
      "address" -> addressRW.definition
    )
  }

  "manual conversion" should {
    "convert Person to Json and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.json
      value should be(
        obj(
          "name" -> "Matt Hicks",
          "age" -> 41,
          "address" -> obj("city" -> "San Jose", "state" -> "California")
        )
      )
      val back = value.as[Person]
      back should be(person)
    }
  }
}
