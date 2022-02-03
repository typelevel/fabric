package spec.gen

import fabric.rw.{RW, ReaderWriter}
import org.scalacheck.{Arbitrary, Gen}

sealed trait Enum

object Enum {
  case object Zero extends Enum
  case object One extends Enum

  lazy val values: Vector[Enum] = Vector(Zero, One)

  implicit val arbitraryEnum: Arbitrary[Enum] = Arbitrary(Gen.choose(0, Enum.values.size - 1).map(values.apply))
  implicit val rw: RW[Enum] = ReaderWriter.enumeration(List(Zero, One))
}