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

import fabric._

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

/**
  * Reader provides a simple T => Json wrapper functionality
  */
trait Reader[T] {
  def read(t: T): Json
  def +(that: Reader[T]): Reader[T] = MultiReader(this, that)
}

object Reader {
  import RW._

  implicit def unitR: Reader[Unit] = unitRW
  implicit def valueR: Reader[Json] = valueRW
  implicit def objR: Reader[Obj] = objRW

  implicit def boolR: Reader[Boolean] = boolRW

  implicit def byteR: Reader[Byte] = byteRW
  implicit def shortR: Reader[Short] = shortRW
  implicit def intR: Reader[Int] = intRW
  implicit def longR: Reader[Long] = longRW
  implicit def floatR: Reader[Float] = floatRW
  implicit def doubleR: Reader[Double] = doubleRW
  implicit def bigIntR: Reader[BigInt] = bigIntRW
  implicit def bigDecimalR: Reader[BigDecimal] = bigDecimalRW

  implicit def stringR: Reader[String] = stringRW
  implicit def regexR: Reader[Regex] = regexRW
  implicit def finiteDurationRW: Reader[FiniteDuration] = RW.finiteDurationRW
  implicit def mapR[V: Reader]: Reader[Map[String, V]] = apply[Map[String, V]](_.map { case (key, value) =>
    key -> value.json
  })
  implicit def tuple2R[K: Reader, V: Reader]: Reader[(K, V)] = apply[(K, V)] { t =>
    arr(t._1.json, t._2.json)
  }
  implicit def tuple3R[T1: Reader, T2: Reader, T3: Reader]: Reader[(T1, T2, T3)] = apply[(T1, T2, T3)] { t =>
    arr(t._1.json, t._2.json, t._3.json)
  }
  implicit def tuple4R[T1: Reader, T2: Reader, T3: Reader, T4: Reader]: Reader[(T1, T2, T3, T4)] =
    apply[(T1, T2, T3, T4)] { t =>
      arr(t._1.json, t._2.json, t._3.json, t._4.json)
    }
  implicit def listR[V: Reader]: Reader[List[V]] = apply[List[V]](v => Arr(v.map(_.json).toVector))
  implicit def vectorR[V: Reader]: Reader[Vector[V]] = apply[Vector[V]](v => Arr(v.map(_.json)))
  implicit def setR[T](implicit r: Reader[T]): Reader[Set[T]] = apply[Set[T]](set => Arr(set.map(r.read).toVector))
  implicit def optionR[T](implicit r: Reader[T]): Reader[Option[T]] = apply[Option[T]] {
    case Some(t) => r.read(t)
    case None => Null
  }

  def apply[T](f: T => Json): Reader[T] = new Reader[T] {
    override def read(t: T): Json = f(t)
  }
}
