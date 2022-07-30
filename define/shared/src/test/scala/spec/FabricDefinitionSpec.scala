package spec

import fabric._
import fabric.rw.Convertible
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
      )) should be(DefType.Obj(Map(
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
      )) should be(DefType.Obj(Map(
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
      d should be(DefType.Opt(DefType.Obj(Map(
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
      val definition = DefType.Obj(Map(
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
      val definition = DefType.Obj(Map(
        "name" -> DefType.Str,
        "age" -> DefType.Int
      ))
      val value = obj(
        "name" -> "Jane Doe"
      )
      definition.validate(value) should be(false)
    }
  }
}
