package spec

import fabric._
import fabric.rw._
import testy.Spec

class FabricSpec extends Spec {
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

  "Fabric" should {
    "represent AST properly" in {
      assertEquals(v, obj(
        "name" -> "Matt \"Matteo\" Hicks",
        "age" -> 41.0,
        "numbers" -> List(1, 2, 3),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.\nBox 123",
          "city" -> "San Jose",
          "state" -> "California",
          "zipcode" -> 95136.0
        )
      ))
    }
    "extract the state" in {
      val state = v("address" \ "state")
      assertEquals(state, str("California"))
    }
    "update the hierarchy" in {
      val updated = v.modify("address" \ "state") { value =>
        str("Tennessee")
      }
      assertEquals(updated, obj(
        "name" -> "Matt \"Matteo\" Hicks",
        "age" -> 41.0,
        "numbers" -> List(1, 2, 3),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.\nBox 123",
          "city" -> "San Jose",
          "state" -> "Tennessee",
          "zipcode" -> 95136.0
        )
      ))
    }
    "remove from the hierarchy" in {
      val removed = v.remove("address" \ "state")
      assertEquals(removed, obj(
        "name" -> "Matt \"Matteo\" Hicks",
        "age" -> 41.0,
        "numbers" -> List(1, 2, 3),
        "address" -> obj(
          "street" -> "123 Somewhere Rd.\nBox 123",
          "city" -> "San Jose",
          "zipcode" -> 95136.0
        )
      ))
    }
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
      assertEquals(merged, expected)
    }
    "convert to/from Special" in {
      val s = obj().as[Special]
      s.name should be(None)
      s.age should be(21)
//      s.data should be(None)
    }
  }
}

case class Special(name: Option[String], age: Int = 21, data: Option[Value])

object Special {
  implicit val rw: ReaderWriter[Special] = ccRW
}