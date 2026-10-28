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
import fabric.io._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JVMNumberPrecisionSpec extends AnyWordSpec with Matchers {
  private val values = List(
    "123456789012345678901.234567891",
    "0.000000000000000001",
    "98765432109876543210.000000000000000001",
    "123456789012345678901234567890",
    "1e400",
    "-2.5e-400",
    "100000",
    "2.5"
  ).map(BigDecimal(_))

  private def exact(json: Json, expected: BigDecimal): Unit = {
    json.asBigDecimal.bigDecimal.compareTo(expected.bigDecimal) should be(0)
    ()
  }

  "Number precision on the JVM" should {
    "keep every number through JsoniterParser" in {
      values.foreach(v => exact(JsoniterParser(JsonFormatter.Compact(num(v))), v))
      JsoniterParser("123456789012345678901234567890").asNum.asBigInt should be(BigInt("123456789012345678901234567890"))
      JsoniterParser("41") should be(NumInt(41))
    }
    "keep every number through YAML" in {
      values.foreach { v =>
        val yaml = YamlFormatter(obj("v" -> num(v)))
        val parsed = JsonParser(yaml, Format.Yaml)("v")
        parsed shouldBe a[Num]
        exact(parsed, v)
      }
    }
  }
}
