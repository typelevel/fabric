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

/**
 * Pins the contract of [[fabric.rw.RW.enumeration]] around the parent-qualified
 * wire form fabric adopted in v1.29.x and the legacy-leaf fallback that
 * accompanies it. Documents both the read-side leniency (forward-compat for
 * records persisted under the old leaf-only wire form) and the write-side
 * asymmetry (writes only produce the new form — relevant for downstream
 * consumers that match indexed string values literally).
 */
class RWEnumerationLegacyLeafSpec extends AnyWordSpec with Matchers {

  sealed trait Color

  object Color {
    implicit val rw: RW[Color] = RW.enumeration(List(Red, Green, Blue))

    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
  }

  "RW.enumeration current wire form" should {
    "write a value using the parent-qualified class chain" in {
      // Definition.simpleClassName(getClass.getName) joins the class chain with
      // dots, dropping lowercase (package) segments. The class chain for
      // `Color.Red` here is RWEnumerationLegacyLeafSpec → Color → Red.
      (Color.Red: Color).json should be(str("RWEnumerationLegacyLeafSpec.Color.Red"))
    }
    "round-trip the current wire form" in {
      val json = (Color.Green: Color).json
      json.as[Color] should be(Color.Green)
    }
  }

  "RW.enumeration legacy-leaf fallback" should {
    "decode an old leaf-only wire value to the matching registered case" in {
      // A persisted record written before fabric's class-chain change carried
      // just the leaf name. The current RW resolves it via the in-memory
      // legacy-leaf index so older records continue to deserialize.
      str("Red").as[Color] should be(Color.Red)
    }
    "decode legacy leaf values case-insensitively" in {
      // The legacy-leaf path is case-insensitive (lowercased before lookup) so
      // hand-rolled tooling or older write paths that vary case still resolve.
      str("green").as[Color] should be(Color.Green)
    }
    "throw a clear error for an unknown enumeration value" in {
      a[RWException] should be thrownBy {
        str("Purple").as[Color]
      }
    }
  }

  "RW.enumeration write side" should {
    "always emit the new parent-qualified form" in {
      // Pinning the asymmetry: writes only produce the new form. Downstream
      // consumers that index the literal wire string and then issue term
      // queries against later writes must migrate stored values — fabric's
      // legacy-leaf fallback covers DESERIALIZATION (read at `json.as[T]`),
      // not server-side string matching in external indices.
      val written = (Color.Blue: Color).json
      written should be(str("RWEnumerationLegacyLeafSpec.Color.Blue"))
      written should not be str("Blue")
    }
  }
}
