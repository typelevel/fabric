package spec

import fabric.io.{Format, JsonParser, YamlFormatter}
import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class YamlFormattingSpec extends AnyWordSpec with Matchers {
  "Yaml Formatting" should {
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
          "test5" -> "This is \"quoted\" text.",
          "test6" -> arr(Null)
        )
      )
      val yamlString = YamlFormatter(v)
      val json = JsonParser(yamlString, Format.Yaml)
      json should be(v)
    }
  }
}
