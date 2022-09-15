package spec

import fabric._
import fabric.rw.Convertible
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.collection.immutable.ListMap

class FabricDefinitionSpec extends AnyWordSpec with Matchers {
  "FabricDefinition" should {
    "represent an Int properly" in {
      FabricDefinition(num(5)) should be(DefType.Int)
    }
    "represent a Null properly" in {
      FabricDefinition(Null) should be(DefType.Null)
    }
    "represent an optional Int properly" in {
      FabricDefinition(List(num(5), Null)) should be(DefType.Opt(DefType.Int))
    }
    "represent an optional Int properly starting with Null" in {
      FabricDefinition(List(Null, num(5))) should be(DefType.Opt(DefType.Int))
    }
    "represent a simple obj" in {
      FabricDefinition(obj(
        "name" -> "John Doe",
        "age" -> 50
      )) should be(DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int
      )))
    }
    "represent a simple obj with optional value" in {
      FabricDefinition(List(
        obj(
          "name" -> "John Doe",
          "age" -> 50
        ),
        obj(
          "name" -> "Jane Doe"
        )
      )) should be(DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Opt(DefType.Int)
      )))
    }
    "represent a simple optional obj" in {
      val d = FabricDefinition(List(
        Null,
        obj(
          "name" -> "Jane Doe",
          "age" -> 50
        )
      ))
      d should be(DefType.Opt(DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int
      ))))
    }
    "fail with conflicting types" in {
      assertThrows[RuntimeException](
        FabricDefinition(List(
          obj("name" -> "Bad"),
          num(5)
        ))
      )
    }
    "validate a definition" in {
      val definition = DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Opt(DefType.Int)
      ))
      val value = obj(
        "name" -> "Jane Doe",
        "age" -> 50
      )
      definition.validate(value) should be(true)
    }
    "fail to validate a definition" in {
      val definition = DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int
      ))
      val value = obj(
        "name" -> "Jane Doe"
      )
      definition.validate(value) should be(false)
    }
    "generate a case class based on a definition" in {
      val definition = DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int
      ))
      val generated = FabricGenerator(definition, "com.example.Person")
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be(
        """package com.example
          |
          |import fabric.rw._
          |
          |case class Person(name: String, age: Long)
          |
          |object Person {
          |  implicit val rw: RW[Person] = ccRW
          |}""".stripMargin)
      generated.additional should be(Nil)
    }
    "generate two case classes based on a definition" in {
      val definition = DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int,
        "location" -> DefType.Obj(ListMap(
          "city" -> DefType.Str,
          "state" -> DefType.Str
        ))
      ))
      val generated = FabricGenerator(
        definition,
        "com.example.Person",
        "location" -> "com.example.Location"
      )
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be(
        """package com.example
          |
          |import fabric.rw._
          |
          |case class Person(name: String, age: Long, location: com.example.Location)
          |
          |object Person {
          |  implicit val rw: RW[Person] = ccRW
          |}""".stripMargin)
      generated.additional.length should be(1)
      val location = generated.additional.head
      location.packageName should be(Some("com.example"))
      location.className should be("Location")
      location.code should be("""package com.example
                                |
                                |import fabric.rw._
                                |
                                |case class Location(city: String, state: String)
                                |
                                |object Location {
                                |  implicit val rw: RW[Location] = ccRW
                                |}""".stripMargin)
    }
    "generate two case classes based on a definition with an array" in {
      val definition = DefType.Obj(ListMap(
        "name" -> DefType.Str,
        "age" -> DefType.Int,
        "locations" -> DefType.Arr(DefType.Obj(ListMap(
          "city" -> DefType.Str,
          "state" -> DefType.Str
        )))
      ))
      val generated = FabricGenerator(
        definition,
        "com.example.Person",
        "locations" -> "com.example.Location"
      )
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be(
        """package com.example
          |
          |import fabric.rw._
          |
          |case class Person(name: String, age: Long, locations: Vector[com.example.Location])
          |
          |object Person {
          |  implicit val rw: RW[Person] = ccRW
          |}""".stripMargin)
      generated.additional.length should be(1)
      val location = generated.additional.head
      location.packageName should be(Some("com.example"))
      location.className should be("Location")
      location.code should be(
        """package com.example
          |
          |import fabric.rw._
          |
          |case class Location(city: String, state: String)
          |
          |object Location {
          |  implicit val rw: RW[Location] = ccRW
          |}""".stripMargin)
    }
  }
}
