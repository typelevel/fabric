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

import scala.scalajs.js
import scala.scalajs.js.JSON

object JSJsonParser extends FormatParser {
  override def format: Format = Format.Json

  override def apply(content: String): Json = parse(JSON.parse(content))

  def parse(value: js.Any): Json = value.asInstanceOf[Any] match {
    case null => Null
    case v: js.Array[_] =>
      Arr(v.toVector.map(a => parse(a.asInstanceOf[js.Any])))
    case v: Int => num(v)
    case v: Long => num(v)
    case v: js.BigInt => num(v.toString())
    case v: js.Object =>
      val d = v.asInstanceOf[js.Dictionary[js.Any]]
      d.toMap.map { case (key, value) =>
        key -> parse(value)
      }
    case v: String => str(v)
    case v: Boolean => bool(v)
    case v: Byte => num(v.doubleValue())
    case v: Float => num(v.toDouble)
    case v =>
      throw new RuntimeException(
        s"Unsupported value in parse: $v (${v.getClass})"
      )
  }
}
