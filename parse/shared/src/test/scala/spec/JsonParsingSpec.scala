package spec

import fabric._
import fabric.parse._

class JsonParsingSpec extends munit.FunSuite {
  test("parse a simple use-case") {
    val v = Json.parse("""{"name": "Matt Hicks", "age": 41}""")
    assertEquals(v, obj(
      "name" -> "Matt Hicks",
      "age" -> 41
    ))
  }
}