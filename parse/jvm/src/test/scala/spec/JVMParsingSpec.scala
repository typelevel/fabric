package spec

import hierarchical._
import hierarchical.parse.{Hocon, Properties, XML, Yaml}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JVMParsingSpec extends AnyWordSpec with Matchers {
  "Parsing on the JVM" should {
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
      val v = XML.parse("<test><yaml>yes</yaml></test>")
      v should be(obj(
        "test" -> obj(
          "yaml" -> "yes"
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
  }
}