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

package fabric.rw
import fabric.Json
import fabric.define.DefType

case class EnhancedRW[T](rw: RW[T], preWrite: List[Json => Json] = Nil, postRead: List[(T, Json) => Json] = Nil)
    extends RW[T] {
  override def definition: DefType = rw.definition

  override def write(value: Json): T = {
    val json = preWrite.foldLeft(value)((j, f) => f(j))
    rw.write(json)
  }

  override def read(t: T): Json = {
    val json = rw.read(t)
    postRead.foldLeft(json)((j, f) => f(t, j))
  }

  override def withPreWrite(f: Json => Json): RW[T] = copy(preWrite = preWrite ::: List(f))

  override def withPostRead(f: (T, Json) => Json): RW[T] = copy(postRead = postRead ::: List(f))
}
