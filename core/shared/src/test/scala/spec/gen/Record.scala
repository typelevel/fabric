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

import fabric.rw._
import org.scalacheck.Arbitrary.{arbBool, arbByte, arbDouble, arbFloat, arbInt, arbLong, arbOption, arbShort, arbString, arbitrary}
import org.scalacheck.Arbitrary
import spec.gen.Structure.arbitraryStructure

final case class Record(
  string: String,
  boolean: Boolean,
  byte: Byte,
  short: Short,
  int: Option[Int],
  long: Long,
  float: Float,
  double: Double,
  enumeration: Enum,
  list: List[String],
  map: Map[String, Int],
  structure: Option[Structure],
  none: Option[String]
)

object Record {
  implicit val rw: RW[Record] = RW.gen

  implicit val arbitraryRecord: Arbitrary[Record] = Arbitrary(for {
    string <- arbitrary[String]
      boolean <- arbitrary[Boolean]
      byte <- arbitrary[Byte]
      short <- arbitrary[Short]
      int <- arbitrary[Option[Int]]
      long <- arbitrary[Long]
      float <- arbitrary[Float]
      double <- arbitrary[Double]
      enumeration <- arbitrary[Enum]
      list <- arbitrary[List[String]]
      map <- arbitrary[Map[String, Int]]
      structure <- arbitrary[Option[Structure]]
      none <- arbitrary[Option[String]]
  } yield Record(string, boolean, byte, short, int, long, float, double, enumeration, list, map, structure, none))
}
