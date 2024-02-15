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

package fabric

import fabric.define.DefType
import fabric.rw.RW

sealed trait JsonPathEntry extends Any

object JsonPathEntry {
  implicit val rw: RW[JsonPathEntry] = RW.poly[JsonPathEntry]()(
    Named.rw, Indexed.rw
  )

  case class Named(name: String) extends AnyVal with JsonPathEntry
  object Named {
    implicit val rw: RW[Named] = RW.wrapped[Named](
      key = "name",
      asJson = _.name,
      fromJson = j => Named(j.asString),
      definition = DefType.Str
    )
  }
  case class Indexed(index: Int) extends AnyVal with JsonPathEntry
  object Indexed {
    implicit val rw: RW[Indexed] = RW.wrapped[Indexed](
      key = "index",
      asJson = _.index,
      fromJson = j => Indexed(j.asInt),
      definition = DefType.Int
    )
  }
}
