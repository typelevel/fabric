package spec

import fabric._
import fabric.rw._

class RWSpecAuto extends munit.FunSuite {
  implicit val addressRW: ReaderWriter[Address] = ccRW[Address]
  implicit val personRW: ReaderWriter[Person] = ccRW[Person]

  test("convert Person to Value and back") {
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
  test("convert from empty obj to Defaults") {
    val v = obj()
    val d = v.as[Defaults]
    assertEquals(d.name, "John Doe")
    assertEquals(d.age, 21)
  }
  test("convert from single argument to Defaults") {
    val v = obj("name" -> "Jane Doe")
    val d = v.as[Defaults]
    assertEquals(d.name, "Jane Doe")
    assertEquals(d.age, 21)
  }
}
