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

import fabric._
import fabric.dsl.*
import fabric.io.{Format, JsonParser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JVMParsingSpec extends AnyWordSpec with Matchers {
  "JVM Parsing" should {
    "parse basic YAML" in {
      val json = JsonParser(
        """
          |test:
          |   yaml: "yes"
          |""".stripMargin,
        Format.Yaml
      )
      json should be(obj("test" -> obj("yaml" -> "yes")))
    }
    "parse basic XML" in {
      val json = JsonParser("<test><xml>yes</xml></test>", Format.XML)
      json should be(obj("test" -> obj("xml" -> "yes")))
    }
    "parse basic HOCON" in {
      val json = JsonParser("""test.hocon = "yes"""", Format.Hocon)
      json should be(obj("test" -> obj("hocon" -> "yes")))
    }
    "parse basic Properties" in {
      val json = JsonParser("test.properties=yes", Format.Properties)
      json should be(obj("test" -> obj("properties" -> "yes")))
    }
  }
}
