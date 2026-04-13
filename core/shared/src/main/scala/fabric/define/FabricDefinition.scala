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
  def apply(json: List[Json]): Definition =
    if (json.isEmpty) {
      Definition(DefType.Null)
    } else {
      var gt = apply(json.head)
      json.tail.foreach { t =>
        val g = apply(t)
        gt = gt.merge(g)
      }
      gt
    }

  def apply(json: Json): Definition = json match {
    case Obj(value) => Definition(DefType.Obj(value.map { case (k, v) => k -> apply(v) }))
    case Arr(value, _) => Definition(DefType.Arr(apply(value.toList)))
    case Str(_, _) => Definition(DefType.Str)
    case NumInt(_, _) => Definition(DefType.Int)
    case NumDec(_, _) => Definition(DefType.Dec)
    case Bool(_, _) => Definition(DefType.Bool)
    case Null => Definition(DefType.Null)
  }
}
