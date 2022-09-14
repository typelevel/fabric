package spec

import fabric._
import fabric.filter.{ChainedFilter, RemoveEmptyFilter, RemoveNullsFilter}
import fabric.io.{Format, JsonFormatter, JsonParser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JVMParsingSpec extends AnyWordSpec with Matchers {
  "JVM Parsing" should {
    "parse basic YAML" in {
      val json = JsonParser(
        """
          |test:
          |   yaml: "yes"
          |""".stripMargin, Format.Yaml
      )
      json should be(obj(
        "test" -> obj(
          "yaml" -> "yes"
        )
      ))
    }
    "parse basic XML" in {
      val json = JsonParser("<test><xml>yes</xml></test>", Format.XML)
      json should be(obj(
        "test" -> obj(
          "xml" -> "yes"
        )
      ))
    }
    "parse basic HOCON" in {
      val json = JsonParser("""test.hocon = "yes"""", Format.Hocon)
      json should be(obj(
        "test" -> obj(
          "hocon" -> "yes"
        )
      ))
    }
    "parse basic Properties" in {
      val json = JsonParser("test.properties=yes", Format.Properties)
      json should be(obj(
        "test" -> obj(
          "properties" -> "yes"
        )
      ))
    }
  }
}