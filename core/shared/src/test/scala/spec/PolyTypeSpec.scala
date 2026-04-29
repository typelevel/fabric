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
import fabric.define.{DefType, Definition}
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PolyTypeSpec extends AnyWordSpec with Matchers {
  import PolyTypeSpec._

  "PolyType" should {
    "register subtypes and serialize them" in {
      // register in a fresh test-local PolyType to keep tests isolated
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat), RW.static(Dog))
      val cat: Animal = Cat
      val json = poly.rw.read(cat)
      json("type").asString should be("Cat")
    }
    "round-trip via the registered RW" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat), RW.static(Dog))
      val asJson = poly.rw.read(Dog)
      val back = poly.rw.write(asJson)
      back should be(Dog)
    }
    "expose registered names via the name namespace" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat), RW.static(Dog))
      val names = poly.name.registered.map(_.name)
      names should contain("Cat")
      names should contain("Dog")
    }
    "validate name lookups against registered subtypes" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat))
      poly.name.from("Cat").map(_.name) should be(Some("Cat"))
      poly.name.from("Unknown") should be(None)
    }
    "build PolyName from instance and from compile-time subtype" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat), RW.static(Dog))
      poly.name.of(Cat).name should be("Cat")
      poly.name.of[Cat.type].name should be("Cat")
    }
    "reflect later registrations" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat))
      poly.name.registered.size should be(1)
      poly.register(RW.static(Dog))
      poly.name.registered.size should be(2)
    }
    "round-trip PolyName through JSON" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat))
      val n: PolyName[Animal] = poly.name.of(Cat)
      val json = n.json
      json.asString should be("Cat")
      json.as[PolyName[Animal]].name should be("Cat")
    }
    "expose definition as a Poly with all registered subtypes" in {
      val poly = PolyType[Animal]
      poly.register(RW.static(Cat), RW.static(Dog))
      poly.rw.definition.defType match {
        case DefType.Poly(values) =>
          values.keySet should contain("Cat")
          values.keySet should contain("Dog")
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }
    "have empty registry before any register call" in {
      val poly = PolyType[Animal]
      poly.name.registered should be(Set.empty)
    }
  }
}

object PolyTypeSpec {
  sealed trait Animal
  case object Cat extends Animal
  case object Dog extends Animal
}
