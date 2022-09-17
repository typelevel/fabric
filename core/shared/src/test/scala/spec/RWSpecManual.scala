package spec

import fabric._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap

class RWSpecManual extends AnyWordSpec with Matchers {
  implicit val addressRW: RW[Address] = new ClassRW[Address] {
    override protected def t2Map(t: Address): ListMap[String, Json] = ListMap(
      "city" -> t.city.json,
      "state" -> t.state.json
    )

    override protected def map2T(map: ListMap[String, Json]): Address = Address(
      city = map("city").as[String],
      state = map("state").as[String]
    )
  }
  implicit val personRW: RW[Person] = new ClassRW[Person] {
    override protected def t2Map(t: Person): ListMap[String, Json] = ListMap(
      "name" -> t.name.json,
      "age" -> t.age.json,
      "address" -> t.address.json
    )

    override protected def map2T(map: ListMap[String, Json]): Person = Person(
      name = map("name").as[String],
      age = map("age").as[Int],
      address = map("address").as[Address]
    )
  }

  "manual conversion" should {
    "convert Person to Value and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.json
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
  }
}