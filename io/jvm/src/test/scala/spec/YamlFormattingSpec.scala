/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package spec

import fabric.io.{Format, JsonParser, YamlFormatter}
import fabric._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

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
          "test3" -> arr(1, 2, 3),
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
    "format a minimal JSON file to YAML properly" in {
      verifyToYaml("openapi-minimal")
    }
    "format a simple JSON file to YAML properly" in {
      verifyToYaml("openapi-simple")
    }
    "format a complex JSON file to YAML properly" in {
      verifyToYaml("openapi-tictactoe")
    }
  }

  private def verifyToYaml(name: String): Assertion = {
    val jsonName = s"$name.json"
    val yamlName = s"$name.yml"
    val jsonString = resource(jsonName)
    val yamlString = resource(yamlName)
    val json = JsonParser(jsonString)
    val yaml = YamlFormatter(json)
    yaml should be(yamlString)
  }

  private def resource(name: String): String = {
    val source =
      Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name))
    try
      source.mkString
    finally
      source.close()
  }
}
