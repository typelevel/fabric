package spec

import fabric._
import fabric.parse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonParsingSpec extends AnyWordSpec with Matchers {
  "Json Parsing" should {
    "parse a simple use-case" in {
      val v = JsonParser.parse("""{"name": "Matt Hicks", "age": 41}""")
      v should be(obj(
        "name" -> "Matt Hicks",
        "age" -> 41
      ))
    }
  }
}