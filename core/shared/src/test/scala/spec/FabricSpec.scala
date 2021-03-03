package spec

import fabric._

class FabricSpec extends munit.FunSuite {
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

  test("represent AST properly") {
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
  test("extract the state") {
    val state = v("address" \ "state")
    assertEquals(state, str("California"))
  }
  test("update the hierarchy") {
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
  test("remove from the hierarchy") {
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
  test("properly merge a simple scenario") {
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
}
