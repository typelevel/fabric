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
    "verify type getting works as expected" in {
      val s: fabric.Value = Str("Hello, World!")
      s.getValue(ValueType.Str) should be(Some(Str("Hello, World!")))
      s.getStr should be(Some(Str("Hello, World!")))
      s.getValue(ValueType.Obj) should be(None)
      s.getValue(ValueType.Bool) should be(None)
      s.getValue(ValueType.Arr) should be(None)
      s.getValue(ValueType.Num) should be(None)
      s.getValue(ValueType.Null) should be(None)
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
      s.data should be(None)
    }
    "use polymorphic values" in {
      val json1 = obj("type" -> "blank")
      val json2 = obj("type" -> "polyValue", "s" -> "Hello, World!")

      val p1 = json1.as[Polymorphic]
      p1 should be(Polymorphic.Blank)
      p1.toValue should be(json1)

      val p2 = json2.as[Polymorphic]
      p2 should be(Polymorphic.PolyValue("Hello, World!"))
      p2.toValue should be(json2)
    }
  }
}

case class Special(name: Option[String], age: Int = 21, data: Option[Value])

object Special {
  implicit val rw: ReaderWriter[Special] = ccRW
}

sealed trait Polymorphic

object Polymorphic {
  implicit val rw: ReaderWriter[Polymorphic] = polyRW[Polymorphic]() {
    case "blank" => staticRW(Blank)
    case "polyValue" => PolyValue.rw
  }

  case object Blank extends Polymorphic

  case class PolyValue(s: String) extends Polymorphic

  object PolyValue {
    implicit val rw: ReaderWriter[PolyValue] = ccRW
  }
}