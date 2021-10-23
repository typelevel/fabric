package spec

import fabric._
import fabric.parse.{Hocon, JsonWriter, Properties, XML, Yaml}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JVMParsingSpec extends AnyWordSpec with Matchers {
  "JVM Parsing" should {
    "parse basic YAML" in {
      val v = Yaml.parse(
        """
          |test:
          |   yaml: "yes"
          |""".stripMargin)
      v should be(obj(
        "test" -> obj(
          "yaml" -> "yes"
        )
      ))
    }
    "parse basic XML" in {
      val v = XML.parse("<test><xml>yes</xml></test>")
      v should be(obj(
        "test" -> obj(
          "xml" -> "yes"
        )
      ))
    }
    "parse basic HOCON" in {
      val v = Hocon.parse("""test.hocon = "yes"""")
      v should be(obj(
        "test" -> obj(
          "hocon" -> "yes"
        )
      ))
    }
    "parse basic Properties" in {
      val v = Properties.parse("test.properties=yes")
      v should be(obj(
        "test" -> obj(
          "properties" -> "yes"
        )
      ))
    }
    "format complex JSON" in {
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
      result should be(expected)
    }
  }
}