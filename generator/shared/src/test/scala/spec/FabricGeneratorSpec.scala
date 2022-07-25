package spec

import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FabricGeneratorSpec extends AnyWordSpec with Matchers {
  "FabricGenerator" should {
    "represent an Int properly" in {
      FabricGenerator(num(5)) should be(GenType.Int)
    }
    "represent a Null properly" in {
      FabricGenerator(Null) should be(GenType.Null)
    }
    "represent an optional Int properly" in {
      FabricGenerator(List(num(5), Null)) should be(GenType.Opt(GenType.Int))
    }
    "represent an optional Int properly starting with Null" in {
      FabricGenerator(List(Null, num(5))) should be(GenType.Opt(GenType.Int))
    }
    "represent a simple obj" in {
      FabricGenerator(obj(
        "name" -> "John Doe",
        "age" -> 50
      )) should be(GenType.Obj(Map(
        "name" -> GenType.Str,
        "age" -> GenType.Int
      )))
    }
    "represent a simple obj with optional value" in {
      FabricGenerator(List(
        obj(
          "name" -> "John Doe",
          "age" -> 50
        ),
        obj(
          "name" -> "Jane Doe"
        )
      )) should be(GenType.Obj(Map(
        "name" -> GenType.Str,
        "age" -> GenType.Opt(GenType.Int)
      )))
    }
    "represent a simple optional obj" in {
      FabricGenerator(List(
        Null,
        obj(
          "name" -> "Jane Doe",
          "age" -> 50
        )
      )) should be(GenType.Opt(GenType.Obj(Map(
        "name" -> GenType.Str,
        "age" -> GenType.Int
      ))))
    }
    "fail with conflicting types" in {
      assertThrows[RuntimeException](
        FabricGenerator(List(
          obj("name" -> "Bad"),
          num(5)
        ))
      )
    }
  }
}
