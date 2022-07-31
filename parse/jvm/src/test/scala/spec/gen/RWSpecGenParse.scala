package spec.gen

import fabric.Json
import fabric.parse.JsonParser
import fabric.rw._
import org.scalacheck.Arbitrary
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RWSpecGenParse extends AnyWordSpec with Checkers {
  "generated automatic conversion" should {
    "serialize and deserialize Value" in {
      implicit val vg: Arbitrary[Json] = ValueGenerator.arbitraryValue
      check { (value: Json) =>
        val json = JsonParser.format(value)
        value == JsonParser.parse(json)
      }
    }
  }
}