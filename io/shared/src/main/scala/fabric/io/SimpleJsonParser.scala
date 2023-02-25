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

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Dead simple Json parser meant to be faster than larger alternatives. Still
  * needs some work to be faster.
  */
object SimpleJsonParser extends FormatParser {
  override def format: Format = Format.Json

  override def apply(content: String): Json = {
    def o(t: (Json, Int)): (Option[Json], Int) = (Some(t._1), t._2)
    def parse(start: Int): (Option[Json], Int) = content.charAt(start) match {
      case '{' => o(parseObj(start + 1))
      case '"' => o(parseString(start + 1))
      case '[' => o(parseArr(start + 1))
      case 't' => o((bool(true), start + 4))
      case 'f' => o((bool(false), start + 5))
      case 'n' => o((Null, start + 4))
      case ']' | '}' => (None, start + 1)
      case c if c.isDigit => o(parseNumber(start))
      case c if c.isWhitespace | c == ',' => parse(start + 1)
      case c => throw new RuntimeException(
          s"Unsupported Json start: $c (offset: $start) - $content"
        )
    }

    def parseString(
      start: Int,
      escape: Boolean = false,
      offset: Int = 0,
      b: mutable.StringBuilder = new mutable.StringBuilder
    ): (Json, Int) = {
      val char = content.charAt(start + offset)
      if (!escape && char == '"') {
        (Str(b.toString()), start + offset + 1)
      } else {
        val esc =
          if (escape) {
            // TODO: Re-evaluate this
            if (char == 'n') {
              b.append('\n')
            } else {
              b.append(char)
            }
            false
          } else if (char == '\\') {
            true
          } else {
            b.append(char)
            false
          }
        parseString(start, esc, offset + 1, b)
      }
    }

    def parseKey(offset: Int): (Option[String], Int) = parse(offset) match {
      case (Some(Str(key)), off) =>
        val colonIndex = content.indexOf(':', off)
        assert(
          colonIndex != -1,
          s"Unable to find colon after key in object! (key: $key, value: ${content.substring(off)})"
        )
        (Some(key), colonIndex + 1)
      case (None, off) => (None, off)
      case (Some(json), _) => throw new RuntimeException(s"Expected key, but got: $json")
    }
    def parseArr(offset: Int): (Json, Int) = {
      var list = List.empty[Json]
      var adjust = offset
      @tailrec
      def recurse(start: Int): Unit = parse(start) match {
        case (None, off) => adjust = off // Finished
        case (Some(json), off) =>
          list = json :: list
          recurse(off)
      }
      recurse(offset)
      (Arr(list.reverse.toVector), adjust)
    }
    def parseObj(offset: Int): (Json, Int) = {
      var list = List.empty[(String, Json)]
      var adjust = offset
      @tailrec
      def recurse(start: Int): Unit = parseKey(start) match {
        case (None, off1) => adjust = off1 // Finished
        case (Some(key), off1) => parse(off1) match {
            case (Some(json), off2) =>
              list = key -> json :: list
              recurse(off2)
            case (None, _) => throw new RuntimeException(
                s"Unable to find value for key: $key"
              )
          }
      }
      recurse(offset)
      (Obj(list.reverse: _*), adjust)
    }
    def parseNumber(offset: Int): (Json, Int) = content.substring(offset).takeWhile(c => c.isDigit || c == '.') match {
      case s if s.contains('.') => (num(BigDecimal(s)), offset + s.length)
      case s => (num(s.toLong), offset + s.length)
    }

    parse(0)._1.getOrElse(throw new RuntimeException(s"Not valid JSON: $content"))
  }
}
