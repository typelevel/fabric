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
import fabric.io.{JsonEscaper, JsonFormatter, JsonParser}
import org.apache.commons.text.StringEscapeUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class JsonEscapeSpec extends AnyWordSpec with Matchers {
  // the escaping the JVM writer used before, kept here as the oracle
  private def oldQuote(s: String): String = s""""${StringEscapeUtils.escapeJson(s)}""""

  // Str.escape as it was before its fast path
  private def oldStrEscape(s: String): String = {
    val b = new java.lang.StringBuilder(s.length + 8)
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      c match {
        case '\b' => b.append("\\b")
        case '\f' => b.append("\\f")
        case '\n' => b.append("\\n")
        case '\r' => b.append("\\r")
        case '\t' => b.append("\\t")
        case '\\' => b.append("\\\\")
        case '"' => b.append("\\\"")
        case _ if c < ' ' =>
          val hex = Integer.toHexString(c.toInt)
          b.append("\\u")
          var pad = 4 - hex.length
          while (pad > 0) {
            b.append('0')
            pad -= 1
          }
          b.append(hex)
        case _ => b.append(c)
      }
      i += 1
    }
    b.toString
  }

  private def check(s: String): Unit = {
    val expected = oldQuote(s)
    val quoted = JsonEscaper.quote(s)
    if (quoted != expected) fail(s"quote differs for ${s.map(_.toInt.toHexString).mkString(" ")}: $quoted vs $expected")
    JsonEscaper.escape(s) should be(expected.substring(1, expected.length - 1))
    JsonFormatter.Compact(Str(s)) should be(expected)
    JsonFormatter.Default(Str(s)) should be(expected)
    val strEscaped = Str.escape(s)
    if (strEscaped != oldStrEscape(s)) fail(s"Str.escape differs for ${s.map(_.toInt.toHexString).mkString(" ")}")
  }

  private val random = new Random(4242)

  private val alphabet: Vector[Char] = Vector('a', 'Z', '0', ' ', '"', '\\', '/', '\b', '\f', '\n', '\r', '\t', '\u0000',
    '\u0001', '\u001b', '\u001f', '~', '\u007f', '\u0080', 'é', 'ÿ', 'Ā', ' ', ' ', '€', '퟿', '\ud800', '\udbff',
    '\udc00', '\udfff', '', '﻿', '￾', '￿')

  private def randomString(length: Int): String = {
    val b = new java.lang.StringBuilder(length)
    while (b.length < length) random.nextInt(10) match {
      case 0 => b.appendCodePoint(0x10000 + random.nextInt(0x100000)) // a valid surrogate pair
      case 1 => b.append(alphabet(random.nextInt(alphabet.length)))
      case 2 => b.append((0xd800 + random.nextInt(0x800)).toChar) // a lone or mismatched surrogate
      case 3 => b.append(random.nextInt(0x10000).toChar)
      case _ => b.append((' ' + random.nextInt(95)).toChar)
    }
    b.toString
  }

  "JSON string escaping" should {
    "write every BMP character as the previous escaper did, alone and inside text" in
      (0 until 0x10000).foreach { i =>
        val c = i.toChar
        check(c.toString)
        check(s"ab${c}cd")
        check(s"$c$c")
      }
    "write every surrogate pair and lone surrogate as the previous escaper did" in {
      val rnd = new Random(7)
      (0 until 20000).foreach { _ =>
        val pair = new String(Character.toChars(0x10000 + rnd.nextInt(0x100000)))
        check(pair)
        check(s"x$pair")
        check(s"${pair.charAt(1)}${pair.charAt(0)}")
        check(s"${pair.charAt(0)}")
        check(s"x${pair.charAt(0)}")
        check(s"${pair.charAt(1)}x")
      }
      check("😀\ud83d")
      check("\ude00😀")
    }
    "write random and long strings as the previous escaper did" in {
      check("")
      check("plain ascii only, nothing to escape at all")
      check("x" * 100000)
      check(("x" * 5000) + "\"" + ("y" * 5000))
      (0 until 5000).foreach(_ => check(randomString(random.nextInt(64))))
      (0 until 50).foreach(_ => check(randomString(1000 + random.nextInt(20000))))
    }
    "escape keys the same as values, compact and pretty, and read them back" in
      (0 until 200).foreach { _ =>
        val key = s"k${randomString(random.nextInt(32))}"
        val value = randomString(random.nextInt(64))
        val json = obj(key -> str(value), "n" -> arr(str(value), num(1)))
        val compact = JsonFormatter.Compact(json)
        compact should be(s"""{${oldQuote(key)}:${oldQuote(value)},"n":[${oldQuote(value)},1]}""")
        val pretty = JsonFormatter.Default(json)
        pretty should include(s"${oldQuote(key)}: ${oldQuote(value)}")
        JsonParser(compact) should be(json)
        JsonParser(pretty) should be(json)
      }
  }
}
