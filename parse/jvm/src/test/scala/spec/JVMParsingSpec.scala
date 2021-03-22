package spec

import fabric._
import fabric.parse.{Hocon, Properties, XML, Yaml}

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
}