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

trait JsonFormatterConfig {
  def newLine(): String

  def indent(depth: Int): String

  def keyValueSeparator(): String = ": "

  def encodeString(s: String): String = {
    val e = JsonParser.escapeJson(s)
    s""""$e""""
  }
}

object JsonFormatterConfig {
  case class Standard(indent: Int = 2) extends JsonFormatterConfig {
    override def newLine(): String = "\n"

    override def indent(depth: Int): String = "".padTo(depth * this.indent, ' ')
  }

  case object Compact extends JsonFormatterConfig {
    override def newLine(): String = ""

    override def indent(depth: Int): String = ""

    override def keyValueSeparator(): String = ":"
  }
}
