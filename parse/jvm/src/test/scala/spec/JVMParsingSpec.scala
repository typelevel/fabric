package spec

import fabric._
import fabric.parse.{Hocon, JsonWriter, Properties, XML, Yaml}

class JVMParsingSpec extends munit.FunSuite {
  test("parse basic YAML") {
    val v = Yaml.parse(
      """
        |test:
        |   yaml: "yes"
        |""".stripMargin)
    assertEquals(v, obj(
      "test" -> obj(
        "yaml" -> "yes"
      )
    ))
  }
  test("parse basic XML") {
    val v = XML.parse("<test><xml>yes</xml></test>")
    assertEquals(v, obj(
      "test" -> obj(
        "xml" -> "yes"
      )
    ))
  }
  test("parse basic HOCON") {
    val v = Hocon.parse("""test.hocon = "yes"""")
    assertEquals(v, obj(
      "test" -> obj(
        "hocon" -> "yes"
      )
    ))
  }
  test("parse basic Properties") {
    val v = Properties.parse("test.properties=yes")
    assertEquals(v, obj(
      "test" -> obj(
        "properties" -> "yes"
      )
    ))
  }
  test("format complex JSON") {
    val v = obj(
      "one" -> 1,
      "two" -> false,
      "three" -> 3.5,
      "four" -> obj(
        "test1" -> "Testing 1",
        "test2" -> Null,
        "test3" -> arr(
          1,
          2,
          3
        ),
        "test4" ->
          """This
            |is
            |multi-line""".stripMargin,
        "test5" -> "This is \"quoted\" text."
      )
    )
    val result = JsonWriter(compact = true)(v)
    val expected = """{"one":1.0,"two":false,"three":3.5,"four":{"test1":"Testing 1","test3":[1.0,2.0,3.0],"test4":"This\nis\nmulti-line","test5":"This is \"quoted\" text.","test2":null}}"""
    assertEquals(result, expected)
  }
}