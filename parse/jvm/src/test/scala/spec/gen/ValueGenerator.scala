package spec.gen

import fabric.{Arr, Bool, NumDec, Obj, Str, Value}
import org.scalacheck.{Arbitrary, Gen}

object ValueGenerator {
  lazy val arbitraryValue: Arbitrary[Value] = Arbitrary(Gen.recursive[Value] { recurse =>
    Gen.oneOf(
      Gen.resultOf(Str(_)),
      Gen.resultOf(NumDec),
      Gen.resultOf(Bool),
      Gen.listOfN[Value](2, recurse).map(_.toVector).map(Arr),
      Gen.mapOfN(2, Gen.zip(Arbitrary.arbitrary[String], recurse)).map(Obj(_))
    )
  })
}
