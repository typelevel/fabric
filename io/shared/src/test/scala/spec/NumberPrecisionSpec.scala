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
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NumberPrecisionSpec extends AnyWordSpec with Matchers {
  private val thirtyDigits = BigDecimal("123456789012345678901.234567891")
  private val tiny = BigDecimal("0.000000000000000001")
  private val balance = BigDecimal("98765432109876543210.000000000000000001")
  private val beyondLong = BigInt("123456789012345678901234567890")

  private def exact(json: Json, expected: BigDecimal): Unit = {
    json.asBigDecimal.bigDecimal.compareTo(expected.bigDecimal) should be(0)
    ()
  }

  private def roundTrip(json: Json): Json = JsonParser(JsonFormatter.Compact(json))

  "Number precision" should {
    "keep a 30-digit decimal through the parser" in {
      exact(JsonParser("123456789012345678901.234567891"), thirtyDigits)
      exact(JsonParser("-123456789012345678901.234567891"), -thirtyDigits)
    }
    "keep a 30-digit decimal through the formatter and back" in {
      exact(roundTrip(num(thirtyDigits)), thirtyDigits)
      exact(JsonParser(JsonFormatter.Default(obj("v" -> num(thirtyDigits))))("v"), thirtyDigits)
    }
    "keep 1e-18 with exact digits" in {
      exact(JsonParser("0.000000000000000001"), tiny)
      exact(JsonParser("1e-18"), tiny)
      exact(JsonParser("1E-18"), tiny)
      exact(roundTrip(num(tiny)), tiny)
    }
    "keep 1e-18 added to a large balance" in {
      exact(JsonParser("98765432109876543210.000000000000000001"), balance)
      exact(roundTrip(num(balance)), balance)
    }
    "keep an integer beyond Long" in {
      JsonParser("123456789012345678901234567890").asNum.asBigInt should be(beyondLong)
      JsonParser("-123456789012345678901234567890").asNum.asBigInt should be(-beyondLong)
      roundTrip(num(BigDecimal(beyondLong))).asNum.asBigInt should be(beyondLong)
      JsonParser("[9223372036854775808]").asVector.head.asNum.asBigInt should be(BigInt("9223372036854775808"))
    }
    "keep a number a Double cannot hold" in {
      exact(JsonParser("1e20"), BigDecimal("1e20"))
      exact(JsonParser("[-1.5e19, 2]").asVector.head, BigDecimal("-1.5e19"))
      exact(JsonParser("1e400"), BigDecimal("1e400"))
      exact(roundTrip(num(BigDecimal("1e400"))), BigDecimal("1e400"))
      exact(roundTrip(num(BigDecimal("-2.5e-400"))), BigDecimal("-2.5e-400"))
    }
    "keep a BigDecimal field of a case class" in {
      val amount = Amount(thirtyDigits, tiny, balance, beyondLong)
      val json = amount.json
      roundTrip(json).as[Amount] should be(amount)
      JsonParser(JsonFormatter.Default(json)).as[Amount] should be(amount)
    }
    "keep precise numbers beside ordinary ones in one document" in {
      val parsed = JsonParser("""{"a": 1.0, "b": 2.5, "c": 41, "d": "12345678901234567890", "e": 123456789012345678901.234567891}""")
      parsed("b") should be(NumDec(BigDecimal("2.5")))
      parsed("c") should be(NumInt(41))
      parsed("d") should be(Str("12345678901234567890"))
      exact(parsed("e"), thirtyDigits)
    }
    "build NumInt for integers within Long and NumDec for decimals" in {
      JsonParser("41") shouldBe a[NumInt]
      JsonParser("-9223372036854775808") should be(NumInt(Long.MinValue))
      JsonParser("9223372036854775807") should be(NumInt(Long.MaxValue))
      JsonParser("2.5") shouldBe a[NumDec]
      JsonParser("123456789012345678901.234567891") shouldBe a[NumDec]
    }
    "read Double-typed fields as before" in {
      val values = Doubles(0.1, 3.5, -2.0e-5, 1.7976931348623157e308, 4.9e-324, 123456.789)
      roundTrip(values.json).as[Doubles] should be(values)
      JsonParser("""{"a": 0.1, "b": 3.5, "c": -2.0E-5, "d": 1.7976931348623157E308, "e": 4.9E-324, "f": 123456.789}""")
        .as[Doubles] should be(values)
    }
  }

  case class Amount(big: BigDecimal, tiny: BigDecimal, balance: BigDecimal, count: BigInt)

  object Amount {
    implicit val rw: RW[Amount] = RW.gen
  }

  case class Doubles(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double)

  object Doubles {
    implicit val rw: RW[Doubles] = RW.gen
  }
}
