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

package fabric.io

import fabric._

import scala.scalajs.js
import scala.scalajs.js.JSON

object JSJsonParser extends FormatParser {
  override def format: Format = Format.Json

  // JSON.parse holds every number as a Double, so a document with a number a Double cannot hold exactly is read by
  // SimpleJsonParser instead, keeping the same NumInt and NumDec choices
  override def apply(content: String): Json =
    if (doublesHold(content)) parse(JSON.parse(content))
    else SimpleJsonParser.parse(content, wholeDecimalsAsInt = true)

  // True when every number in the content has at most 15 significant digits, an exponent within 290 and at most 290
  // characters, so its Double is the nearest to its decimal value and reads back as that decimal
  private def doublesHold(content: String): Boolean = {
    val n = content.length
    var i = 0
    var holds = true
    while (holds && i < n) {
      val c = content.charAt(i)
      if (c == '"') {
        i += 1
        while (i < n && content.charAt(i) != '"') i += (if (content.charAt(i) == '\\') 2 else 1)
        i += 1
      } else if (c == '-' || (c >= '0' && c <= '9')) {
        val start = i
        var significant = 0
        var exponent = 0
        var inExponent = false
        while (i < n && numberChar(content.charAt(i))) {
          val d = content.charAt(i)
          if (d == 'e' || d == 'E') inExponent = true
          else if (d >= '0' && d <= '9') {
            if (inExponent) exponent = math.min(exponent * 10 + (d - '0'), 10000)
            else if (significant > 0 || d != '0') significant += 1
          }
          i += 1
        }
        holds = significant <= 15 && exponent < 290 && i - start <= 290
      } else {
        i += 1
      }
    }
    holds
  }

  private def numberChar(c: Char): Boolean =
    (c >= '0' && c <= '9') || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E'

  def parse(value: js.Any): Json = value.asInstanceOf[Any] match {
    case null => Null
    case v: js.Array[_] => Arr(v.toVector.map(a => parse(a.asInstanceOf[js.Any])))
    case v: Int => num(v)
    case v: Long => num(v)
    case v: js.BigInt => num(v.toString())
    case v: js.Object =>
      val d = v.asInstanceOf[js.Dictionary[js.Any]]
      d.toMap.map { case (key, value) => key -> parse(value) }
    case v: String => str(v)
    case v: Boolean => bool(v)
    case v: Double if v.isWhole && v >= -9.223372036854775808e18 && v < 9.223372036854775808e18 => num(v.toLong)
    case v: Double => num(v)
    case v: Byte => num(v.doubleValue())
    case v: Float => num(v.toDouble)
    case v => throw new RuntimeException(
        s"Unsupported value in parse: $v (${v.getClass})"
      )
  }
}
