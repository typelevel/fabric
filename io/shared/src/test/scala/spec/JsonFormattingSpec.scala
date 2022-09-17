package spec

import fabric.io.{Format, JsonFormatter}
import fabric._
import fabric.io._
import fabric.rw._
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
      val expected = """{"one":1,"two":false,"three":3.5,"four":{"test1":"Testing 1","test2":null,"test3":[1,2,3],"test4":"This\nis\nmulti-line","test5":"This is \"quoted\" text.","test6":[null]}}"""
      jsonString should be(expected)
    }
    "parse a String with as support" in {
      val s = """{
        | "value": "Hello, World!"
        |}""".stripMargin.as[Simple](Format.Json)
      s should be(Simple("Hello, World!"))
    }
    "parse a Array[Byte] with as support" in {
      val array =
        """{
          | "value": "Hello, World!"
          |}""".stripMargin.getBytes("UTF-8")
      val s = array.as[Simple](Format.Json)
      s should be(Simple("Hello, World!"))
    }
  }

  case class Simple(value: String)

  object Simple {
    implicit val rw: RW[Simple] = ccRW
  }
}
