package spec

import fabric.io.JsonFormatter
import fabric.{Null, arr, obj}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonFormattingSpec extends AnyWordSpec with Matchers {
  "Json Formatting" should {
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
      val jsonString = JsonFormatter.Compact(v)
      val expected = """{"one":1,"two":false,"three":3.5,"four":{"test6":[null],"test1":"Testing 1","test3":[1,2,3],"test4":"This\nis\nmulti-line","test5":"This is \"quoted\" text.","test2":null}}"""
      jsonString should be(expected)
    }
  }
}
