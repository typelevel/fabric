package spec.gen

import fabric.rw._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

final case class Structure(
  value: String
)

object Structure {
  implicit val rw: RW[Structure] = RW.gen

  implicit val arbitraryStructure: Arbitrary[Structure] = Arbitrary(for {
    value <- arbitrary[String]
  } yield Structure(value))
}
