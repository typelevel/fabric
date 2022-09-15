package spec.gen

import fabric.{Arr, Bool, Json, NumDec, Obj, Str}
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.ListMap

object ValueGenerator {
  lazy val arbitraryValue: Arbitrary[Json] = Arbitrary(Gen.recursive[Json] { recurse =>
    Gen.oneOf(
      Gen.resultOf(Str(_)),
      Gen.resultOf(NumDec(_)),
      Gen.resultOf(Bool(_)),
      Gen.listOfN[Json](2, recurse).map(_.toVector).map(Arr(_)),
      Gen.listOfN[(String, Json)](2, Gen.zip(Arbitrary.arbitrary[String], recurse)).map(list => Obj(ListMap.from(list)))
    )
  })
}