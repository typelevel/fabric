package spec

import cats.effect.testing.scalatest.AsyncIOSpec
import fabric._
import fabric.io._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}

class JsonParsingSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "Json Parsing" should {
    "parse a simple use-case" in {
      JsonParser("""{"name": "Matt Hicks", "age": 41}""", Format.Json).map { json =>
        json should be(obj(
          "name" -> "Matt Hicks",
          "age" -> 41
        ))
      }
    }
  }
}