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

import com.fasterxml.jackson.core.{JsonFactory, JsonToken, JsonParser => JParser}
import fabric.{Arr, Bool, Json, Null, NumDec, NumInt, Obj, Str}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object JacksonParser extends FormatParser {
  private lazy val factory = new JsonFactory()
    .enable(JParser.Feature.ALLOW_COMMENTS)
    .enable(JParser.Feature.ALLOW_SINGLE_QUOTES)
    .enable(JParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    .enable(JParser.Feature.ALLOW_YAML_COMMENTS)

  override def format: Format = Format.Json

  override def apply(content: String): Json = {
    val parser = factory.createParser(content)
    try {
      parse(parser)
    } finally {
      parser.close()
    }
  }

  protected def parse(parser: JParser): Json = parseToken(parser, parser.nextToken())

  private def parseToken(parser: JParser, token: JsonToken): Json = token match {
    case JsonToken.START_OBJECT => parseObj(parser, ListMap.empty)
    case JsonToken.START_ARRAY => parseArr(parser, Vector.empty)
    case JsonToken.VALUE_STRING => Str(parser.getValueAsString)
    case JsonToken.VALUE_NUMBER_FLOAT => NumDec(BigDecimal(parser.getValueAsDouble))
    case JsonToken.VALUE_NUMBER_INT => NumInt(parser.getValueAsLong)
    case JsonToken.VALUE_NULL => Null
    case JsonToken.VALUE_TRUE => Bool(true)
    case JsonToken.VALUE_FALSE => Bool(false)
    case t => throw new RuntimeException(s"Unsupported token: $t")
  }

  @tailrec
  private def parseObj(parser: JParser, map: ListMap[String, Json]): Obj = {
    val next = parser.nextToken()
    if (next == JsonToken.END_OBJECT) {
      Obj(map)
    } else {
      val key = parser.getCurrentName
      val value = parse(parser)
      parseObj(parser, map + (key -> value))
    }
  }

  @tailrec
  private def parseArr(parser: JParser, vector: Vector[Json]): Arr = {
    val next = parser.nextToken()
    if (next == JsonToken.END_ARRAY) {
      Arr(vector)
    } else {
      val value = parseToken(parser, next)
      parseArr(parser, vector :+ value)
    }
  }
}