package spec

import fabric._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CryoSpec extends AnyWordSpec with Matchers {
  "Cryo" should {
    def freezeAndThaw(json: Json): Assertion = {
      val bb = Cryo.freeze(json, allocateDirect = true)
      bb.flip()
      val thawed = Cryo.thaw(bb)
      thawed should be(json)
    }

    "freeze and thaw a Str properly" in {
      val json = Str("Hello, World!")
      freezeAndThaw(json)
    }
    "freeze and thaw a simple obj" in {
      val json = obj(
        "value" -> 5
      )
      freezeAndThaw(json)
    }
    "freeze and thaw a complex obj" in {
      val json = obj(
        "name" -> "Matt \"Matteo\" Hicks",
        "age" -> 41,
        "numbers" -> List(1, 2, 3),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.\nBox 123",
          "city" -> "San Jose",
          "state" -> "California",
          "zipcode" -> 95136
        )
      )
      freezeAndThaw(json)
    }
  }
}
