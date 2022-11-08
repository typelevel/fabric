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

import fabric.{Arr, Bool, Json, Null, NumDec, NumInt, Obj, Str}

case class JsonFormatter(config: JsonFormatterConfig) extends Formatter {
  override def format: Format = Format.Json

  def apply(value: Json): String = write(value, 0)

  private def write(value: Json, depth: Int): String = value match {
    case Arr(v) =>
      val content = v
        .map { value =>
          s"${config.newLine()}${config.indent(depth + 1)}${write(value, depth + 1)}"
        }
        .mkString(",")
      s"[$content${config.newLine()}${config.indent(depth)}]"
    case Bool(b) => b.toString
    case Null => "null"
    case NumInt(n) => n.toString
    case NumDec(n) => n.toString()
    case Obj(map) =>
      val content = map.toList
        .map { case (key, value) =>
          s"${config.newLine()}${config.indent(depth + 1)}${config.encodeString(key)}${config
              .keyValueSeparator()}${write(value, depth + 1)}"
        }
        .mkString(",")
      s"{$content${config.newLine()}${config.indent(depth)}}"
    case Str(s) => config.encodeString(s)
  }
}

object JsonFormatter {
  lazy val Default: JsonFormatter = JsonFormatter(
    JsonFormatterConfig.Standard()
  )
  lazy val Compact: JsonFormatter = JsonFormatter(JsonFormatterConfig.Compact)
}
