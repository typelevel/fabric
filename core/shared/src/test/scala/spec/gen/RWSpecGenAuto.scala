package spec.gen

import fabric.rw._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class RWSpecGenAuto extends AnyWordSpec with Checkers {
  "generated automatic conversion" should {
    "convert Record to Value and back" in {
      check { (record: Record) =>
        val value = record.toValue
        record.equals(value.as[Record])
      }
    }
  }
}