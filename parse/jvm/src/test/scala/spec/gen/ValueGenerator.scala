package spec.gen

import fabric.{Arr, Bool, NumDec, Obj, Str, Json}
import org.scalacheck.{Arbitrary, Gen}

object ValueGenerator {
  lazy val arbitraryValue: Arbitrary[Json] = Arbitrary(Gen.recursive[Json] { recurse =>
    Gen.oneOf(
      Gen.resultOf(Str(_)),
      Gen.resultOf(NumDec(_)),
      Gen.resultOf(Bool(_)),
      Gen.listOfN[Json](2, recurse).map(_.toVector).map(Arr(_)),
      Gen.mapOfN(2, Gen.zip(Arbitrary.arbitrary[String], recurse)).map(Obj(_))
    )
  })
}