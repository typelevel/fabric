package spec

import fabric._
import fabric.io._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonParsingSpec extends AnyWordSpec with Matchers {
  "Json Parsing" should {
    "parse a simple use-case" in {
      val json = JsonParser("""{"name": "Matt Hicks", "age": 41}""", Format.Json)
      json should be(obj(
        "name" -> "Matt Hicks",
        "age" -> 41
      ))
    }
  }
}