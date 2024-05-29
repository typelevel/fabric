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

import fabric.Json
import org.apache.commons.text.StringEscapeUtils

import java.io.File
import java.nio.file.Path
import scala.io.Source

/**
  * Json provides convenience functionality to parse and format JSON to/from
  * fabric Values
  */
object JsonParser extends MultiFormatParser {
  override def parsers: List[FormatParser] = List(HoconParser, JacksonParser, PropertiesParser, XMLParser, YamlParser)

  def apply(file: File, format: Format): Json = apply(Source.fromFile(file, "UTF-8"), format)
  def apply(path: Path, format: Format): Json = apply(Source.fromFile(path.toFile, "UTF-8"), format)

  def apply(file: File): Json = apply(file, Format.Json)
  def apply(path: Path): Json = apply(path, Format.Json)
  def apply(content: String): Json = apply(content, Format.Json)

  private[io] def escapeJson(s: String): String = StringEscapeUtils.escapeJson(s)
}
