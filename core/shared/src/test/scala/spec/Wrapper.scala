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
import fabric.define.DefType
import fabric.rw._
import izumi.reflect.Tag

import scala.annotation.nowarn

case class Wrapper[T](name: String, value: T, other: Option[T])

@nowarn
object Wrapper {
  // TODO: Fix this!
  implicit def rw[T](implicit trw: RW[T], tag: Tag[T]): RW[Wrapper[T]] = RW.gen
//  implicit def rw[T](implicit trw: RW[T], tag: Tag[T]): RW[Wrapper[T]] = RW.from[Wrapper[T]](
//    r = w => obj(
//      "name" -> w.name,
//      "value" -> trw.read(w.value),
//      "other" -> w.other.json
//    ),
//    w = j => Wrapper[T](
//      name = j("name").asString,
//      value = trw.write(j("value")),
//      other = j.get("other").flatMap(_.as[Option[T]])
//    ),
//    d = DefType.Dynamic
//  )
}
