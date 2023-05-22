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
  * Writable provides a simple Json => T wrapper functionality
  */
trait Writer[T] {
  def write(value: Json): T
  def +(that: Writer[T])(implicit merge: (T, T) => T): Writer[T] = MultiWriter(this, that)
}

object Writer {
  import RW._

  implicit def unitW: Writer[Unit] = unitRW
  implicit def valueW: Writer[Json] = valueRW
  implicit def objW: Writer[Obj] = objRW

  implicit def boolW: Writer[Boolean] = boolRW

  implicit def byteR: Writer[Byte] = byteRW
  implicit def shortR: Writer[Short] = shortRW
  implicit def intW: Writer[Int] = intRW
  implicit def longW: Writer[Long] = longRW
  implicit def floatW: Writer[Float] = floatRW
  implicit def doubleW: Writer[Double] = doubleRW
  implicit def bigIntW: Writer[BigInt] = bigIntRW
  implicit def bigDecimalW: Writer[BigDecimal] = bigDecimalRW

  implicit def stringW: Writer[String] = stringRW
  implicit def regexW: Writer[Regex] = regexRW
  implicit def finiteDurationW: Writer[FiniteDuration] = finiteDurationRW
  implicit def mapStringW[V: Writer]: Writer[Map[String, V]] = apply[Map[String, V]] { v =>
    v.asObj.value.map { case (key, value) => key -> value.as[V] }
  }
  implicit def mapW[K: Writer, V: Writer]: Writer[Map[K, V]] = apply[Map[K, V]] { json =>
    json.asVector.map { j =>
      val map = j.asMap
      map("key").as[K] -> map("value").as[V]
    }.toMap
  }
  implicit def tuple2W[K: Writer, V: Writer]: Writer[(K, V)] = apply[(K, V)] { j =>
    j.asVector match {
      case Vector(k, v) => (k.as[K], v.as[V])
      case v => throw new RuntimeException(s"Invalid shape for tuple2: $v")
    }
  }
  implicit def tuple3W[T1: Writer, T2: Writer, T3: Writer]: Writer[(T1, T2, T3)] = apply[(T1, T2, T3)] { j =>
    j.asVector match {
      case Vector(t1, t2, t3) => (t1.as[T1], t2.as[T2], t3.as[T3])
      case v => throw new RuntimeException(s"Invalid shape for tuple3: $v")
    }
  }
  implicit def tuple4W[T1: Writer, T2: Writer, T3: Writer, T4: Writer]: Writer[(T1, T2, T3, T4)] =
    apply[(T1, T2, T3, T4)] { j =>
      j.asVector match {
        case Vector(t1, t2, t3, t4) => (t1.as[T1], t2.as[T2], t3.as[T3], t4.as[T4])
        case v => throw new RuntimeException(s"Invalid shape for tuple4: $v")
      }
    }
  implicit def listW[V: Writer]: Writer[List[V]] = apply[List[V]](v => v.asVector.map(_.as[V]).toList)
  implicit def vectorW[V: Writer]: Writer[Vector[V]] = apply[Vector[V]](v => v.asVector.map(_.as[V]))
  implicit def setW[T](implicit w: Writer[T]): Writer[Set[T]] = apply[Set[T]] {
    case Arr(vector) => vector.toSet.map(w.write)
    case v => throw new RuntimeException(s"Unsupported set: $v")
  }
  implicit def optionW[T](implicit w: Writer[T]): Writer[Option[T]] = apply[Option[T]] {
    case Null => None
    case v => Option(w.write(v))
  }

  def apply[T](f: Json => T): Writer[T] = new Writer[T] {
    override def write(value: Json): T = f(value)
  }
}
