package spec.gen

import cats.effect.unsafe.implicits.global
import fabric.Json
import fabric.io.{Format, JsonFormatter, JsonParser}
import org.scalacheck.Arbitrary
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RWSpecGenParse extends AnyWordSpec with Checkers {
  "generated automatic conversion" should {
    "serialize and deserialize Value" in {
      implicit val vg: Arbitrary[Json] = ValueGenerator.arbitraryValue
      check { (value: Json) =>
        val json = JsonFormatter.Default(value).unsafeRunSync()
        value == JsonParser(json, Format.Json).unsafeRunSync()
      }
    }
  }
}