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
        case DefType.Poly(values, _) =>
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

  "PolyType.commonFields" should {

    "be empty when no subtypes are registered" in {
      val poly = PolyType[Shape]
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          p.commonFields should be(empty)
          p.values should be(empty)
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }

    "include every field of a single registered subtype (intersection of one set is itself)" in {
      val poly = PolyType[Shape]
      poly.register(summon[RW[Circle]])
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          // Single subtype: commonFields = the whole subtype's field map.
          p.commonFields.keySet should be(Set("kind", "radius"))
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }

    "intersect across subtypes — keep names every subtype carries" in {
      val poly = PolyType[Shape]
      poly.register(summon[RW[Circle]], summon[RW[Square]])
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          // Both Circle and Square declare `kind: String`. Circle's
          // `radius` and Square's `side` are subtype-specific; they
          // drop out of the intersection.
          p.commonFields.keySet should be(Set("kind"))
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }

    "shrink commonFields when a new subtype registers without one of the previously-common fields" in {
      val poly = PolyType[Shape]
      poly.register(summon[RW[Circle]])
      // After Circle alone: kind + radius are both common (single-subtype).
      poly.register(summon[RW[Square]])
      // After Square joins: only `kind` survives — radius is Circle-only.
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          p.commonFields.keySet should be(Set("kind"))
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }

    "drop a name where subtypes disagree on the field's type" in {
      val poly = PolyType[Mismatch]
      poly.register(summon[RW[StringValued]], summon[RW[IntValued]])
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          // Both subtypes have `value` but with different types — the
          // intersection drops it because the abstract parent can't
          // declare a single type for the field.
          p.commonFields should not contain key("value")
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }

    "be empty when any subtype is non-Obj (e.g. case-object enum cases)" in {
      // A poly mixing record-shaped and unit-shaped subtypes has no
      // record-level intersection — the unit subtype contributes no
      // fields.
      val poly = PolyType[MixedShape]
      poly.register(summon[RW[Triangle]], RW.static(MixedShape.Origin))
      poly.rw.definition.defType match {
        case p: DefType.Poly =>
          p.commonFields should be(empty)
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }
  }
}

object PolyTypeSpec {
  sealed trait Animal
  case object Cat extends Animal
  case object Dog extends Animal

  // For commonFields tests — record-shaped subtypes with overlap.
  trait Shape
  case class Circle(kind: String, radius: Double) extends Shape derives RW
  case class Square(kind: String, side: Double) extends Shape derives RW

  // Same field name, different field type — intersection should drop it.
  trait Mismatch
  case class StringValued(value: String) extends Mismatch derives RW
  case class IntValued(value: Int) extends Mismatch derives RW

  // Heterogeneous: one record-shaped, one case-object — no record-level
  // intersection should be possible.
  trait MixedShape
  case class Triangle(sides: Int) extends MixedShape derives RW
  object MixedShape {
    case object Origin extends MixedShape
  }
}
