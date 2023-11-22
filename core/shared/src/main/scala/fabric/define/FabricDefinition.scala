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

package fabric.define

import fabric.{Arr, Bool, Json, Null, NumDec, NumInt, Obj, Str}

object FabricDefinition {
  def apply(json: List[Json]): DefType =
    if (json.isEmpty) {
      DefType.Null
    } else {
      var gt = apply(json.head)
      json.tail.foreach { t =>
        val g = apply(t)
        gt = gt.merge(g)
      }
      gt
    }

  def apply(json: Json): DefType = json match {
    case Obj(value) => DefType.Obj(value.map { case (k, v) => k -> apply(v) }, value.get("className").map(_.asString))
    case Arr(value) => DefType.Arr(apply(value.toList))
    case Str(_) => DefType.Str
    case NumInt(_) => DefType.Int
    case NumDec(_) => DefType.Dec
    case Bool(_) => DefType.Bool
    case Null => DefType.Null
  }
}
