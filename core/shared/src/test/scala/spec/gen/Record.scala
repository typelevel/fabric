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
