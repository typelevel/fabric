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

package spec.gen

import fabric.{Arr, Bool, Json, NumDec, Obj, Str}
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.ListMap

object ValueGenerator {
  lazy val arbitraryValue: Arbitrary[Json] = Arbitrary(
    Gen.recursive[Json] { recurse =>
      Gen.oneOf(
        Gen.resultOf(Str(_)),
        Gen.resultOf(NumDec(_)),
        Gen.resultOf(Bool(_)),
        Gen.listOfN[Json](2, recurse).map(_.toVector).map(Arr(_)),
        Gen
          .listOfN[(String, Json)](
            2,
            Gen.zip(Arbitrary.arbitrary[String], recurse)
          )
          .map(list => Obj(ListMap(list: _*)))
      )
    }
  )
}
