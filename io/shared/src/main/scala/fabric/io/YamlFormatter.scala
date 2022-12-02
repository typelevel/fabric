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

object YamlFormatter extends Formatter {
  override def format: Format = Format.Yaml

  override def apply(json: Json): String = write(json, 0).trim

  private def write(json: Json, depth: Int): String = {
    def pad(adjust: Int = 0): String = "".padTo((depth + adjust) * 2, ' ')
    json match {
      case Arr(v) =>
        v.map(write(_, depth + 1))
          .map { s =>
            s"${pad()}- ${s.dropWhile(_.isWhitespace)}"
          }
          .mkString("\n", "\n", "")
      case Bool(b) => b.toString
      case Null => ""
      case NumInt(n) => n.toString
      case NumDec(n) => n.toString()
      case Obj(map) =>
        map.toList
          .map {
            case (key, value) =>
              val v = write(value, depth + 1) match {
                case s if s.headOption.contains('\n') => s
                case s => s" $s"
              }
              s"${pad()}$key:$v"
          }
          .mkString("\n", "\n", "")
      case Str(s) if s.contains("\n") =>
        s.split('\n').map(s => s"${pad()}$s").mkString("|-\n", "\n", "")
      case Str(s) => s
    }
  }
}
