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

import scala.annotation.unused

object Delta {

  /**
    * Returns a Json object with only the differences as {"old": ???, "new": ???}
    */
  def diff(json1: Json, json2: Json): Option[Json] = delta(json1, json2, internalDiff)

  private def internalDiff(json1: Json, json2: Json): Json = obj(
    "old" -> json1,
    "new" -> json2
  )

  /**
    * Returns a Json object with only the changes between the two objects representing only the new values.
    */
  def changed(json1: Json, json2: Json): Option[Json] = delta(json1, json2, internalChanged)

  private def internalChanged(@unused json1: Json, json2: Json): Json = json2

  private def delta(json1: Json, json2: Json, f: (Json, Json) => Json): Option[Json] =
    if (json1 == json2) {
      None
    } else {
      Some(json1 match {
        case Arr(vector1, _) => json2 match {
            case Arr(vector2, _) =>
              val maxLength = math.max(vector1.length, vector2.length)
              val v = (0 until maxLength).toVector.map { index =>
                val j1 = if (vector1.length > index) vector1(index) else Null
                val j2 = if (vector2.length > index) vector2(index) else Null
                f(j1, j2)
              }
              Arr(v)
            case _ => f(json1, json2)
          }
        case Obj(map1) => json2 match {
            case Obj(map2) =>
              val keys = map1.keySet ++ map2.keySet
              obj(keys.toList.flatMap { key =>
                val j1 = map1.getOrElse(key, Null)
                val j2 = map2.getOrElse(key, Null)
                if (j1 == j2) {
                  None
                } else {
                  Some(key -> f(j1, j2))
                }
              } *)
            case _ => f(json1, json2)
          }
        case _ => f(json1, json2)
      })
    }
}
