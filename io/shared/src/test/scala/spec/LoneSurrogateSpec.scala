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

class LoneSurrogateSpec extends AnyWordSpec with Matchers {
  private val Replacement = "\uFFFD"
  private val Emoji = "\uD83D\uDE00"

  // input -> the string a reader gets back once every unpaired surrogate is U+FFFD
  private val cases: List[(String, String)] = List(
    "\uD83D" -> Replacement,
    "\uDE00" -> Replacement,
    "a\uD83Db" -> s"a${Replacement}b",
    "a\uDE00b" -> s"a${Replacement}b",
    "\uDE00\uD83D" -> s"$Replacement$Replacement",
    "end\uD83D" -> s"end$Replacement",
    "end\uDE00" -> s"end$Replacement",
    "\uD83D\uD83D\uDE00" -> s"$Replacement$Emoji",
    s"$Emoji\uDE00" -> s"$Emoji$Replacement",
    s"\"$Emoji\uD83D\n" -> s"\"$Emoji$Replacement\n"
  )

  private def hasLoneSurrogate(s: String): Boolean = {
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))) i += 2
      else if (Character.isSurrogate(c)) return true
      else i += 1
    }
    false
  }

  private def formatters: List[JsonFormatter] = List(JsonFormatter.Compact, JsonFormatter.Default)

  "Writing a string with an unpaired surrogate" should {
    "write U+FFFD in its place, compact and pretty, and read back" in cases.foreach { case (in, expected) =>
      formatters.foreach { f =>
        val out = f(Str(in))
        hasLoneSurrogate(out) should be(false)
        JsonParser(out) should be(Str(expected))
      }
    }
    "do the same for keys and nested values" in cases.foreach { case (in, expected) =>
      val json = obj(in -> arr(str(in), obj("k" -> str(in))))
      formatters.foreach { f =>
        val out = f(json)
        hasLoneSurrogate(out) should be(false)
        JsonParser(out) should be(obj(expected -> arr(str(expected), obj("k" -> str(expected)))))
      }
    }
    "write U+FFFD from Str.escape and Str.toString" in cases.foreach { case (in, expected) =>
      hasLoneSurrogate(Str.escape(in)) should be(false)
      Str.escape(in) should be(Str.escape(expected))
      Str(in).toString should be(Str(expected).toString)
      JsonParser(Str(in).toString) should be(Str(expected))
    }
    "write U+FFFD in YAML" in cases.foreach { case (in, expected) =>
      val yaml = YamlFormatter(obj("k" -> str(in)))
      hasLoneSurrogate(yaml) should be(false)
      yaml should be(YamlFormatter(obj("k" -> str(expected))))
    }
  }

  "Writing a valid surrogate pair" should {
    "pass it through unchanged" in {
      val s = s"a$Emoji b \uD834\uDD1E c"
      formatters.foreach { f =>
        JsonParser(f(Str(s))) should be(Str(s))
      }
      Str.escape(s) should be(s)
      YamlFormatter(obj("k" -> str(s))) should be(s"k: '$s'")
    }
  }
}
