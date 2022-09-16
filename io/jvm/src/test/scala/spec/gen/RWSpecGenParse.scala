package spec.gen

import fabric.Json
import fabric.io.{Format, JsonFormatter, JsonParser, JsoniterParser}
import org.scalacheck.Arbitrary
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RWSpecGenParse extends AnyWordSpec with Checkers {
  "generated automatic conversion" should {
    "serialize and deserialize Value" in {
      implicit val vg: Arbitrary[Json] = ValueGenerator.arbitraryValue
      check { (value: Json) =>
        val json = JsonFormatter.Default(value)
        // TODO: Switch to JsonParser when JacksonParser can handle this properly
        value == JsoniterParser(json)
      }
    }
  }
}