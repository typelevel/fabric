package spec

import fabric._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RWSpecAuto extends AnyWordSpec with Matchers {
  "automatic conversion" should {
    "convert Person to Value and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.toValue
      value should be(obj(
        "name" -> "Matt Hicks",
        "age" -> 41,
        "address" -> obj(
          "city" -> "San Jose",
          "state" -> "California"
        )
      ))
      val back = value.as[Person]
      back should be(person)
    }
    "convert from empty obj to Defaults" in {
      val v = obj()
      val d = v.as[Defaults]
      d.name should be("John Doe")
      d.age should be(21)
    }
    "convert from single argument to Defaults" in {
      val v = obj("name" -> "Jane Doe")
      val d = v.as[Defaults]
      d.name should be("Jane Doe")
      d.age should be(21)
    }
    "supporting generic type on case class" in {
      val w = Wrapper("Test1", Address("San Jose", "California"), Some(Address("Norman", "Oklahoma")))
      val value = w.toValue
      value should be(obj(
        "name" -> "Test1",
        "value" -> obj(
          "city" -> "San Jose",
          "state" -> "California"
        ),
        "other" -> obj(
          "city" -> "Norman",
          "state" -> "Oklahoma"
        )
      ))
      val w2 = value.as[Wrapper[Address]]
      w2 should be(w)
    }
    "supporting Values in conversions" in {
      val w = Wrapper("Test2", obj("city" -> "San Jose"), Some(obj("city" -> "Norman")))
      val value = w.toValue
      value should be(obj(
        "name" -> "Test2",
        "value" -> obj(
          "city" -> "San Jose"
        ),
        "other" -> obj(
          "city" -> "Norman"
        )
      ))
    }
  }
}