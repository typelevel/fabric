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

/**
  * Writes a string as the JVM's JSON writer always has: the quote, the backslash and `/` escaped, `\b \f \n \r \t` by
  * name, and every other character below U+0020 or from U+007F up (each half of a surrogate pair on its own) as
  * `\uXXXX` in upper-case hex. A string with nothing to escape is copied whole.
  */
object JsonEscaper {
  private val Hex: Array[Char] = "0123456789ABCDEF".toCharArray

  @inline private def needsEscape(c: Char): Boolean = c < ' ' || c >= '\u007f' || c == '"' || c == '\\' || c == '/'

  private def firstEscape(s: String): Int = {
    val n = s.length
    var i = 0
    while (i < n) {
      if (needsEscape(s.charAt(i))) return i
      i += 1
    }
    -1
  }

  def escape(s: String): String = {
    val first = firstEscape(s)
    if (first < 0) s
    else {
      val b = new java.lang.StringBuilder(s.length + 16)
      appendFrom(s, first, b)
      b.toString
    }
  }

  /**
    * `s` escaped between quotes.
    */
  def quote(s: String): String = {
    val first = firstEscape(s)
    val b = new java.lang.StringBuilder(s.length + (if (first < 0) 2 else 18))
    b.append('"')
    if (first < 0) {
      b.append(s)
      ()
    } else appendFrom(s, first, b)
    b.append('"')
    b.toString
  }

  private def appendFrom(s: String, first: Int, b: java.lang.StringBuilder): Unit = {
    b.append(s, 0, first)
    val n = s.length
    var start = first
    var i = first
    while (i < n) {
      val c = s.charAt(i)
      if (needsEscape(c)) {
        if (start < i) {
          b.append(s, start, i)
          ()
        }
        c match {
          case '"' => b.append("\\\"")
          case '\\' => b.append("\\\\")
          case '/' => b.append("\\/")
          case '\b' => b.append("\\b")
          case '\f' => b.append("\\f")
          case '\n' => b.append("\\n")
          case '\r' => b.append("\\r")
          case '\t' => b.append("\\t")
          case _ => b
              .append('\\')
              .append('u')
              .append(Hex((c >> 12) & 0xf))
              .append(Hex((c >> 8) & 0xf))
              .append(Hex((c >> 4) & 0xf))
              .append(Hex(c & 0xf))
        }
        start = i + 1
      }
      i += 1
    }
    if (start < n) {
      b.append(s, start, n)
      ()
    }
  }
}
