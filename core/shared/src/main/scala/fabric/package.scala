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

package object fabric {
  implicit def string2Path(s: String): JsonPath = new JsonPath(List(s))

  implicit def map2Obj(map: Map[String, Json]): Obj = Obj(map)

  implicit def seq2Arr(seq: Seq[Json]): Arr = Arr(seq.toVector)

  implicit def ints2Arr(seq: Seq[Int]): Arr = Arr(seq.map(n => num(n)).toVector)

  implicit def doubles2Arr(seq: Seq[Double]): Arr = Arr(seq.map(num).toVector)

  implicit def string2PathEntry(name: String): JsonPathEntry =
    JsonPathEntry.Named(name)

  implicit def int2PathEntry(index: Int): JsonPathEntry =
    JsonPathEntry.Indexed(index)

  /**
    * Create an Obj from the params
    */
  def obj(params: (String, Json)*): Obj = Obj(params: _*)

  /**
    * Create an Arr from the params
    */
  def arr(values: Json*): Arr = Arr(values.toVector)

  /**
    * Create a Str from the supplied String
    */
  implicit def str(s: String): Str = Str(s)

  /**
    * Create a Num from the supplied String
    */
  def num(value: String): Num = NumDec(BigDecimal(value))

  /**
    * Create a Num from the supplied Double
    */
  implicit def num(value: Double): Num = NumDec(BigDecimal(value))

  /**
    * Create a Num from the supplied BigDecimal
    */
  implicit def num(value: BigDecimal): Num = NumDec(value)

  implicit def num(value: Int): Num = NumInt(value.toLong)

  implicit def num(value: Long): Num = NumInt(value)

  /**
    * Create a Bool from the supplied Boolean
    */
  implicit def bool(b: Boolean): Bool = Bool(b)
}
