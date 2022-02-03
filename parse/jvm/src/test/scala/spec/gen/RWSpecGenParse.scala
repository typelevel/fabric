package spec.gen

import fabric.Value
import fabric.parse.Json
import fabric.rw._
import org.scalacheck.Arbitrary
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RWSpecGenParse extends AnyWordSpec with Checkers {
  "generated automatic conversion" should {
    "serialize and deserialize Value" in {
      implicit val vg: Arbitrary[Value] = ValueGenerator.arbitraryValue
      check { (value: Value) =>
        val json = Json.format(value)
        value == Json.parse(json)
      }
    }
  }
}