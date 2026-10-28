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

import scala.collection.immutable.VectorMap

/**
  * A JSON parser written in Scala that reads every number at full precision: an integer within Long as NumInt, any
  * other number as NumDec holding its exact decimal value.
  */
object SimpleJsonParser extends FormatParser {
  override def format: Format = Format.Json

  override def apply(content: String): Json = parse(content, wholeDecimalsAsInt = false)

  /**
    * @param wholeDecimalsAsInt
    *   reads a decimal whose value is whole and within Long (`1.0`, `1e3`) as NumInt, as a parser holding numbers as
    *   Doubles does
    */
  private[io] def parse(content: String, wholeDecimalsAsInt: Boolean): Json = {
    val reader = new Reader(content, wholeDecimalsAsInt)
    reader.skipWhitespace()
    val json = reader.value()
    reader.skipWhitespace()
    if (reader.offset != content.length) reader.fail("Unexpected content after the value")
    json
  }

  final private class Reader(content: String, wholeDecimalsAsInt: Boolean) {
    var offset: Int = 0

    def fail(message: String): Nothing = throw new RuntimeException(s"$message (offset: $offset) - $content")

    def skipWhitespace(): Unit = while (
      offset < content.length && {
        val c = content.charAt(offset); c == ' ' || c == '\n' || c == '\r' || c == '\t'
      }
    ) offset += 1

    private def expect(c: Char): Unit =
      if (offset < content.length && content.charAt(offset) == c) offset += 1
      else fail(s"Expected '$c'")

    private def literal(word: String, json: Json): Json =
      if (content.startsWith(word, offset)) {
        offset += word.length
        json
      } else {
        fail(s"Expected $word")
      }

    def value(): Json = {
      if (offset >= content.length) fail("Unexpected end of content")
      content.charAt(offset) match {
        case '{' => obj()
        case '[' => arr()
        case '"' => Str(string())
        case 't' => literal("true", Bool(value = true))
        case 'f' => literal("false", Bool(value = false))
        case 'n' => literal("null", Null)
        case c if c == '-' || (c >= '0' && c <= '9') => number()
        case c => fail(s"Unsupported Json start: $c")
      }
    }

    private def obj(): Json = {
      expect('{')
      skipWhitespace()
      var map = VectorMap.empty[String, Json]
      if (offset < content.length && content.charAt(offset) == '}') {
        offset += 1
      } else {
        var more = true
        while (more) {
          skipWhitespace()
          if (offset >= content.length || content.charAt(offset) != '"') fail("Expected a key")
          val key = string()
          skipWhitespace()
          expect(':')
          skipWhitespace()
          map = map.updated(key, value())
          skipWhitespace()
          if (offset < content.length && content.charAt(offset) == ',') offset += 1
          else {
            expect('}')
            more = false
          }
        }
      }
      Obj(map)
    }

    private def arr(): Json = {
      expect('[')
      skipWhitespace()
      val b = Vector.newBuilder[Json]
      if (offset < content.length && content.charAt(offset) == ']') {
        offset += 1
      } else {
        var more = true
        while (more) {
          skipWhitespace()
          b += value()
          skipWhitespace()
          if (offset < content.length && content.charAt(offset) == ',') offset += 1
          else {
            expect(']')
            more = false
          }
        }
      }
      Arr(b.result())
    }

    private def string(): String = {
      expect('"')
      val b = new java.lang.StringBuilder
      var start = offset
      var done = false
      while (!done) {
        if (offset >= content.length) fail("Unterminated string")
        content.charAt(offset) match {
          case '"' =>
            b.append(content, start, offset)
            offset += 1
            done = true
          case '\\' =>
            b.append(content, start, offset)
            if (offset + 1 >= content.length) fail("Unterminated escape")
            content.charAt(offset + 1) match {
              case '"' => b.append('"')
              case '\\' => b.append('\\')
              case '/' => b.append('/')
              case 'b' => b.append('\b')
              case 'f' => b.append('\f')
              case 'n' => b.append('\n')
              case 'r' => b.append('\r')
              case 't' => b.append('\t')
              case 'u' =>
                if (offset + 6 > content.length) fail("Incomplete unicode escape")
                val hex = content.substring(offset + 2, offset + 6)
                val code =
                  try Integer.parseInt(hex, 16)
                  catch { case _: NumberFormatException => fail(s"Invalid unicode escape: \\u$hex") }
                b.append(code.toChar)
                offset += 4
              case c => fail(s"Invalid escape: \\$c")
            }
            offset += 2
            start = offset
          case _ => offset += 1
        }
      }
      b.toString
    }

    private def digits(): Unit = {
      val start = offset
      while (offset < content.length && { val c = content.charAt(offset); c >= '0' && c <= '9' }) offset += 1
      if (offset == start) fail("Expected a digit")
    }

    private def number(): Json = {
      val start = offset
      if (content.charAt(offset) == '-') offset += 1
      digits()
      var decimal = false
      if (offset < content.length && content.charAt(offset) == '.') {
        offset += 1
        digits()
        decimal = true
      }
      if (offset < content.length && (content.charAt(offset) == 'e' || content.charAt(offset) == 'E')) {
        offset += 1
        if (offset < content.length && (content.charAt(offset) == '+' || content.charAt(offset) == '-')) offset += 1
        digits()
        decimal = true
      }
      val text = content.substring(start, offset)
      val bd = new java.math.BigDecimal(text)
      val whole = !decimal || (wholeDecimalsAsInt && (bd.signum() == 0 || bd.stripTrailingZeros().scale() <= 0))
      if (whole && bd.precision() - bd.scale() <= 19 && bd.toBigInteger.bitLength() < 64) NumInt(bd.longValueExact())
      else NumDec(BigDecimal.exact(bd))
    }
  }
}
