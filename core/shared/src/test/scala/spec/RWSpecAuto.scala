package spec

import fabric._
import fabric.rw._
import testy.Spec

class RWSpecAuto extends Spec {
  implicit val addressRW: ReaderWriter[Address] = ccRW[Address]
  implicit val personRW: ReaderWriter[Person] = ccRW[Person]

  "automatic conversion" should {
    "convert Person to Value and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.toValue
      assertEquals(value, obj(
        "name" -> "Matt Hicks",
        "age" -> 41.0,
        "address" -> obj(
          "city" -> "San Jose",
          "state" -> "California"
        )
      ))
      val back = value.as[Person]
      assertEquals(back, person)
    }
    "convert from empty obj to Defaults" in {
      val v = obj()
      val d = v.as[Defaults]
      assertEquals(d.name, "John Doe")
      assertEquals(d.age, 21)
    }
    "convert from single argument to Defaults" in {
      val v = obj("name" -> "Jane Doe")
      val d = v.as[Defaults]
      assertEquals(d.name, "Jane Doe")
      assertEquals(d.age, 21)
    }
    "supporting generic type on case class" in {
      val w = Wrapper("Test1", Address("San Jose", "California"), Some(Address("Norman", "Oklahoma")))
      val value = w.toValue
      assertEquals(value, obj(
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
  }
}

case class Wrapper[T](name: String, value: T, other: Option[T])

object Wrapper {
  implicit def rw[T](implicit trw: ReaderWriter[T]): ReaderWriter[Wrapper[T]] = ccRW[Wrapper[T]]
}