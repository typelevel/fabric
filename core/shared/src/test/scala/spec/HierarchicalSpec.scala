package spec

import hierarchical._

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HierarchicalSpec extends AnyWordSpec with Matchers {
  "Hierarchical" when {
    "creating objects" should {
      val v: Obj = obj(
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

      "output to string as proper JSON" in {
        v.toString should be("""{"name": "Matt \"Matteo\" Hicks", "age": 41.0, "numbers": [1.0, 2.0, 3.0], "address": {"street": "123 Somewhere Rd.\nBox 123", "city": "San Jose", "state": "California", "zipcode": 95136.0}}""")
      }
      "extract the state" in {
        val state = v("address" \ "state")
        state should be(str("California"))
      }
      "update the hierarchy" in {
        val updated = v.modify("address" \ "state") { value =>
          str("Tennessee")
        }
        updated.toString should be("""{"name": "Matt \"Matteo\" Hicks", "age": 41.0, "numbers": [1.0, 2.0, 3.0], "address": {"street": "123 Somewhere Rd.\nBox 123", "city": "San Jose", "state": "Tennessee", "zipcode": 95136.0}}""")
      }
      "remove from the hierarchy" in {
        val removed = v.remove("address" \ "state")
        removed.toString should be("""{"name": "Matt \"Matteo\" Hicks", "age": 41.0, "numbers": [1.0, 2.0, 3.0], "address": {"street": "123 Somewhere Rd.\nBox 123", "city": "San Jose", "zipcode": 95136.0}}""")
      }
    }
    "merging objects" should {
      "properly merge a simple scenario" in {
        val v1 = obj(
          "name" -> "Matt Hicks",
          "age" -> 41,
          "numbers" -> List(1, 2, 3),
          "address" -> obj(
            "street" -> "123 Somewhere Rd.",
            "city" -> "San Jose"
          )
        )
        val v2 = obj(
          "age" -> 42,
          "numbers" -> List(4, 5, 6),
          "address" -> obj(
            "state" -> "California"
          )
        )
        val merged = v1.merge(v2)
        val expected = obj(
          "name" -> "Matt Hicks",
          "age" -> 42,
          "numbers" -> List(4, 5, 6),
          "address" -> obj(
            "street" -> "123 Somewhere Rd.",
            "city" -> "San Jose",
            "state" -> "California"
          )
        )
        merged should be(expected)
      }
    }
  }
}
