package spec.gen

import org.scalacheck.{Arbitrary, Gen}
import spec.gen.Enum.Enum

object Enum:

  enum Enum:

    case Zero
    case One

  implicit val arbitraryEnum: Arbitrary[Enum] = Arbitrary(Gen.choose(0, Enum.values.size - 1).map(Enum.fromOrdinal))

  def fromOrdinal(ordinal: Int): Enum = Enum.fromOrdinal(ordinal)

  def toOrdinal(value: Enum): Int = value.ordinal
