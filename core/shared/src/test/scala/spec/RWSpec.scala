package spec

import hierarchical._
import hierarchical.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RWSpec extends AnyWordSpec with Matchers {
  "ReaderWriter" when {
    "converting manually" should {
      implicit val addressRW: ReaderWriter[Address] = new ClassRW[Address] {
        override protected def t2Map(t: Address): Map[String, Value] = Map(
          "city" -> t.city.toValue,
          "state" -> t.state.toValue
        )

        override protected def map2T(map: Map[String, Value]): Address = Address(
          city = map("city").as[String],
          state = map("state").as[String]
        )
      }
      implicit val personRW: ReaderWriter[Person] = new ClassRW[Person] {
        override protected def t2Map(t: Person): Map[String, Value] = Map(
          "name" -> t.name.toValue,
          "age" -> t.age.toValue,
          "address" -> t.address.toValue
        )

        override protected def map2T(map: Map[String, Value]): Person = Person(
          name = map("name").as[String],
          age = map("age").as[Int],
          address = map("address").as[Address]
        )
      }

      "convert Person to Value and back" in {
        val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
        val value = person.toValue
        value should be(obj(
          "name" -> "Matt Hicks",
          "age" -> 41.0,
          "address" -> obj(
            "city" -> "San Jose",
            "state" -> "California"
          )
        ))
        val back = value.as[Person]
        back should be(person)
      }
    }
    "converting automatically via macro" should {
      implicit val addressRW: ReaderWriter[Address] = ccRW[Address]
      implicit val personRW: ReaderWriter[Person] = ccRW[Person]

      "convert Person to Value and back" in {
        val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
        val value = person.toValue
        value should be(obj(
          "name" -> "Matt Hicks",
          "age" -> 41.0,
          "address" -> obj(
            "city" -> "San Jose",
            "state" -> "California"
          )
        ))
        val back = value.as[Person]
        back should be(person)
      }
    }
    "converting automatically via macro with defaults" should {
      implicit val rw: ReaderWriter[Defaults] = ccRW

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
    }
  }

  case class Person(name: String, age: Int, address: Address)
  case class Address(city: String, state: String)
  case class Defaults(name: String = "John Doe", age: Int = 21)
}
