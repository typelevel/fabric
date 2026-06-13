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

/**
 * Verifies that `mapType`-derived transforms (`leaf`, `lowerCase`) apply consistently to BOTH the
 * wire value AND the `definition` (the `DefType.Poly` variant keys), so the generated schema can't
 * drift from what's actually written. Covered for the two `DefType.Poly` producers: `enumeration`
 * (whole-value `Str` discriminator) and `poly` (a `"type"` field inside an `Obj`).
 */
class RWMapTypeSpec extends AnyWordSpec with Matchers {
  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    val qualified: RW[Color] = RW.enumeration(List(Red, Green, Blue))
    val leaf: RW[Color] = qualified.leaf
    val leafLower: RW[Color] = qualified.leaf.lowerCase
  }

  sealed trait Shape
  object Shape {
    case class Circle(radius: Int) extends Shape
    case class Square(side: Int) extends Shape

    val circleRW: RW[Circle] = RW.gen[Circle]
    val squareRW: RW[Square] = RW.gen[Square]
    val leaf: RW[Shape] = RW.poly[Shape]()(circleRW, squareRW).leaf
  }

  private def polyKeys(rw: RW[_]): Set[String] = rw.definition.defType match {
    case DefType.Poly(values, _) => values.keySet
    case other                   => fail(s"expected a Poly definition, got: $other")
  }

  "leaf on an enumeration" should {
    "write the leaf form" in {
      Color.leaf.read(Color.Red) should be(str("Red"))
    }
    "round-trip the leaf form" in {
      Color.leaf.write(str("Green")) should be(Color.Green)
      Color.leaf.write(Color.leaf.read(Color.Blue)) should be(Color.Blue)
    }
    "reflect the leaf names in the definition" in {
      polyKeys(Color.leaf) should be(Set("Red", "Green", "Blue"))
    }
  }

  "leaf.lowerCase on an enumeration" should {
    "write the lowercased leaf form" in {
      Color.leafLower.read(Color.Red) should be(str("red"))
    }
    "round-trip the lowercased leaf form" in {
      Color.leafLower.write(str("red")) should be(Color.Red)
      Color.leafLower.write(Color.leafLower.read(Color.Green)) should be(Color.Green)
    }
    "reflect the lowercased leaf names in the definition" in {
      polyKeys(Color.leafLower) should be(Set("red", "green", "blue"))
    }
  }

  "leaf on a poly/sealed trait" should {
    "write the leaf discriminator in the type field" in {
      Shape.leaf.read(Shape.Circle(3)).asObj.value("type") should be(str("Circle"))
    }
    "round-trip via the leaf discriminator" in {
      val circle: Shape = Shape.Circle(3)
      Shape.leaf.write(Shape.leaf.read(circle)) should be(circle)
    }
    "reflect the leaf names in the definition" in {
      polyKeys(Shape.leaf) should be(Set("Circle", "Square"))
    }
  }
}
