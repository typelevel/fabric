package spec.gen

import fabric.rw._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

final case class Structure(
  value: String
)

object Structure {
  implicit val rw: RW[Structure] = ccRW

  implicit val arbitraryStructure: Arbitrary[Structure] = Arbitrary(for {
    value <- arbitrary[String]
  } yield Structure(value))
}
