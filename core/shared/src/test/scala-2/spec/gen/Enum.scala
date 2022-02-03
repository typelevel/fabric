package spec.gen

import fabric.rw.{RW, ReaderWriter}
import org.scalacheck.{Arbitrary, Gen}

object Enum extends Enumeration {
  type Enum = Value

  val Zero, One = Value

  implicit val arbitraryEnum: Arbitrary[Enum] = Arbitrary(Gen.choose(0, Enum.values.size - 1).map(Enum.fromOrdinal))
  implicit val rw: RW[Enum] = ReaderWriter.enumeration(List(Zero, One), _.id.toString)

  def fromOrdinal(ordinal: Int): Enum = this.values.toSeq(ordinal)

  def toOrdinal(value: Enum): Int = value.id
}