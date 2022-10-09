package spec

import fabric.io.SimpleJsonParser
import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SimpleJsonParserSpec extends AnyWordSpec with Matchers {
  "SimpleJsonParser" should {
    "parse a simple String" in {
      val json = SimpleJsonParser(""""name"""")
      json should be(str("name"))
    }
    "parse a simple integer" in {
      val json = SimpleJsonParser("215")
      json should be(num(215))
    }
    "parse a simple decimal" in {
      val json = SimpleJsonParser("2.15")
      json should be(num(2.15))
    }
    "parse a simple array" in {
      val json = SimpleJsonParser("[1, 2, 3]")
      json should be(arr(1, 2, 3))
    }
    "parse a simple object" in {
      val json = SimpleJsonParser("""{"name": "Testing"}""")
      json should be(obj("name" -> "Testing"))
    }
    "parse a slightly more complex object" in {
      val json = SimpleJsonParser("""{"name": "Matt Hicks", "age": 41}""")
      json should be(obj(
        "name" -> "Matt Hicks",
        "age" -> 41
      ))
    }
  }
}
