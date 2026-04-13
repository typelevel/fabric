/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package spec

import fabric._
import fabric.dsl.*
import fabric.define.{DefType, Definition, Format, GenericType}
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Scala3Spec extends AnyWordSpec with Matchers {
  final case class Address(city:String, state: String) derives RW
  final case class Person(name: String, age: Int, address: Address) derives RW

  "Scala 3 Specific Functionality" should {
    "use derives to convert Person to Json and back" in {
      val person = Person("Matt Hicks", 41, Address("San Jose", "California"))
      val value = person.json
      value should be(
        obj(
          "name" -> "Matt Hicks",
          "age" -> 41,
          "address" -> obj("city" -> "San Jose", "state" -> "California")
        )
      )
      val back = value.as[Person]
      back should be(person)
    }
    "handle built-in enums" in {
      Color.Green.json should be(str("Green"))
      str("Green").as[Color] should be(Color.Green)
    }
    "handle enums with derives RW" in {
      Direction.North.json should be(str("North"))
      str("South").as[Direction] should be(Direction.South)
    }
    "round-trip all enum values with derives RW" in {
      Direction.values.foreach { d =>
        str(d.toString).as[Direction] should be(d)
        d.json should be(str(d.toString))
      }
    }
    "handle union types (A | B) serialization" in {
      import UnionTest._
      val catVal: Cat | Dog = Cat("Whiskers", 3)
      val dogVal: Cat | Dog = Dog("Rex", "Golden")

      val catJson = catVal.json
      catJson should be(obj("type" -> "Cat", "name" -> "Whiskers", "age" -> 3))

      val dogJson = dogVal.json
      dogJson should be(obj("type" -> "Dog", "name" -> "Rex", "breed" -> "Golden"))
    }
    "handle union types (A | B) deserialization" in {
      import UnionTest._
      val catJson = obj("type" -> "Cat", "name" -> "Whiskers", "age" -> 3)
      val dogJson = obj("type" -> "Dog", "name" -> "Rex", "breed" -> "Golden")

      catJson.as[Cat | Dog] should be(Cat("Whiskers", 3))
      dogJson.as[Cat | Dog] should be(Dog("Rex", "Golden"))
    }
    "handle union types round-trip identity" in {
      import UnionTest._
      val cat = Cat("Luna", 5)
      val dog = Dog("Max", "Labrador")

      (cat: Cat | Dog).json.as[Cat | Dog] should be(cat)
      (dog: Cat | Dog).json.as[Cat | Dog] should be(dog)
    }
    "handle three-way union types (A | B | C)" in {
      import UnionTest._
      val cat: Cat | Dog | Fish = Cat("Milo", 2)
      val dog: Cat | Dog | Fish = Dog("Buddy", "Poodle")
      val fish: Cat | Dog | Fish = Fish("Nemo", saltwater = true)

      val catJson = cat.json
      catJson should be(obj("type" -> "Cat", "name" -> "Milo", "age" -> 2))

      val fishJson = fish.json
      fishJson should be(obj("type" -> "Fish", "name" -> "Nemo", "saltwater" -> true))

      catJson.as[Cat | Dog | Fish] should be(Cat("Milo", 2))
      dog.json.as[Cat | Dog | Fish] should be(Dog("Buddy", "Poodle"))
      fishJson.as[Cat | Dog | Fish] should be(Fish("Nemo", saltwater = true))
    }
    "handle union types with generic collision — deserialization distinguishes by _generic" in {
      import GenericCollisionTest._
      type T = Box[String] | Box[Int]
      given RW[T] = RW.gen

      // Box[String] serializes with value as a string, Box[Int] with value as a number
      val strBox = Box[String]("hello")
      val intBox = Box[Int](42)

      // Serialize via concrete RWs (not the union) to get correct _generic
      val strJson = Box.rw[String].read(strBox)
      val intJson = Box.rw[Int].read(intBox)

      // Verify _generic is present and different
      strJson("_generic")("T")("type").asString should be("string")
      intJson("_generic")("T")("type").asString should be("numeric")

      // Wrap with type field as the union would
      val unionStr = strJson.asObj.merge(Obj("type" -> Str("Box"))).asObj
      val unionInt = intJson.asObj.merge(Obj("type" -> Str("Box"))).asObj

      // Deserialize through union RW — _generic disambiguates
      val restored1 = unionStr.as[T]
      restored1 should be(Box("hello"))

      val restored2 = unionInt.as[T]
      restored2 should be(Box(42))

      // Prove disambiguation worked: Box[Int] deserialized the value as Int, not String
      // If it used the wrong child RW, value would be "42" (String) instead of 42 (Int)
      restored2.asInstanceOf[Box[Int]].value should be(42)
      restored1.asInstanceOf[Box[String]].value should be("hello")
    }
    "handle enums with parameters (ADT enums)" in {
      import ParameterizedEnumTest._
      val rgb: Shape = Shape.Circle(5.0)
      val rect: Shape = Shape.Rectangle(3.0, 4.0)

      val rgbJson = rgb.json
      rgbJson should be(obj("type" -> "Circle", "radius" -> 5.0))

      val rectJson = rect.json
      rectJson should be(obj("type" -> "Rectangle", "width" -> 3.0, "height" -> 4.0))

      rgbJson.as[Shape] should be(Shape.Circle(5.0))
      rectJson.as[Shape] should be(Shape.Rectangle(3.0, 4.0))
    }
    "reject unknown _type in union deserialization" in {
      import UnionTest._
      val badJson = obj("type" -> "Parrot", "name" -> "Polly")
      an[RWException] should be thrownBy badJson.as[Cat | Dog]
    }
    "include field path in deserialization errors" in {
      import ErrorTest._

      val badJson = obj("name" -> "test", "inner" -> obj("value" -> "not_a_number"))
      val ex = intercept[RuntimeException] { badJson.as[Outer] }
      // Should be wrapped with field path context
      (ex.getMessage should (include("inner") or include("Inner") or include("value")))
    }
    "report clear error for non-object JSON" in {
      import ErrorTest._
      val ex = the[RWException] thrownBy str("oops").as[Simple]
      ex.getMessage should include("Expected JSON object")
    }
    "handle custom type field name via @typeField annotation" in {
      import CustomFieldTest._
      val circle: CustomShape = CustomShape.Round(5.0)
      val json = circle.json
      json should be(obj("kind" -> "Round", "radius" -> 5.0))
      json.as[CustomShape] should be(CustomShape.Round(5.0))
    }
    "reject union type deserialization without type discriminator" in {
      import UnionTest._
      val ambiguousJson = obj("name" -> "Mystery", "age" -> 7)
      an[RWException] should be thrownBy ambiguousJson.as[Cat | Dog]
    }
    "handle mixed enums with parameterized and simple cases" in {
      import MixedEnumTest._
      val circle: Shape = Shape.Circle(5.0)
      val point: Shape = Shape.Point

      val circleJson = circle.json
      circleJson should be(obj("type" -> "Circle", "radius" -> 5.0))

      val pointJson = point.json
      pointJson should be(obj("type" -> "Point"))

      circleJson.as[Shape] should be(Shape.Circle(5.0))
      pointJson.as[Shape] should be(Shape.Point)

      // Round-trip
      (Shape.Rectangle(3.0, 4.0): Shape).json.as[Shape] should be(Shape.Rectangle(3.0, 4.0))
    }
    "include @serialized members in JSON output" in {
      import SerializedTest._
      val person = NamedPerson("Matt", "Hicks")
      val json = person.json
      json should be(obj("firstName" -> "Matt", "lastName" -> "Hicks", "fullName" -> "Matt Hicks"))

      // Deserialization ignores the extra field
      val back = obj("firstName" -> "Jane", "lastName" -> "Doe").as[NamedPerson]
      back should be(NamedPerson("Jane", "Doe"))
      back.fullName should be("Jane Doe")
    }
    "support @serialized with custom key name" in {
      import SerializedTest._
      val v = WithCustomKey(5)
      val json = v.json
      json should be(obj("value" -> 5, "computed_double" -> 10))
    }
    "include @serialized fields in DefType definition" in {
      import SerializedTest._
      val defn = implicitly[RW[NamedPerson]].definition
      defn.defType match {
        case DefType.Obj(map) =>
          map.keys should contain("fullName")
        case other => fail(s"Expected DefType.Obj, got: $other")
      }
    }
    "exclude @transient fields from serialization" in {
      import TransientTest._
      val config = Config("myapp", "super-secret")
      val json = config.json
      json should be(obj("name" -> "myapp"))
      json.get("secret") should be(None)

      // Deserialization uses the default value
      val back = obj("name" -> "otherapp").as[Config]
      back should be(Config("otherapp", "default"))
    }
    "proxy AnyVal case class to inner type RW" in {
      import AnyValTest._
      val id = UserId("abc-123")
      val json = id.json
      json should be(str("abc-123"))

      val back = str("xyz-789").as[UserId]
      back should be(UserId("xyz-789"))
    }
    "populate genericTypes for generic case classes" in {
      import GenericTypeTest._
      val wrapperDef = Wrapper.rw[String].definition
      wrapperDef.genericTypes.length should be(1)
      wrapperDef.genericTypes.head.name should be("T")
      wrapperDef.genericTypes.head.definition.defType should be(DefType.Str)

      val wrapperIntDef = Wrapper.rw[Int].definition
      wrapperIntDef.genericTypes.length should be(1)
      wrapperIntDef.genericTypes.head.name should be("T")
      wrapperIntDef.genericTypes.head.definition.defType should be(DefType.Int)

      val pairDef = Pair.rw[String, Int].definition
      pairDef.genericTypes.length should be(2)
      pairDef.genericTypes(0).name should be("A")
      pairDef.genericTypes(0).definition.defType should be(DefType.Str)
      pairDef.genericTypes(1).name should be("B")
      pairDef.genericTypes(1).definition.defType should be(DefType.Int)
    }
    "verify genericName on fields for Pair[String, Int]" in {
      import GenericTypeTest._
      val pairDef = Pair.rw[String, Int].definition
      val fields = pairDef.defType.asInstanceOf[DefType.Obj].map
      fields("first").genericName should be(Some("A"))
      fields("second").genericName should be(Some("B"))
    }
    "populate genericTypes for AnyVal wrapper" in {
      import AnyValTest._
      val idDef = UserId.given_RW_UserId.definition
      idDef.className should be(Some("spec.AnyValTest.UserId"))
      idDef.genericTypes should be(Nil)
    }
    "have empty genericTypes for non-generic case classes" in {
      spec.Person.rw.definition.genericTypes should be(Nil)
    }
    "populate format from @format annotation" in {
      import FormatTest._
      val d = Contact.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("email").format should be(Format.Email)
      fields("website").format should be(Format.Uri)
      fields("name").format should be(Format.Raw)
    }
    "populate deprecated from @fieldDeprecated annotation" in {
      import DeprecatedTest._
      val d = ApiResponse.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("oldField").deprecated should be(true)
      fields("newField").deprecated should be(false)
    }
    "populate defaultValue for fields with defaults" in {
      import DefaultTest._
      val d = Config.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("host").defaultValue should be(Some(str("localhost")))
      fields("port").defaultValue should be(Some(num(8080)))
      fields("name").defaultValue should be(None)
    }
    "round-trip Definition with format, defaultValue, and deprecated" in {
      val original = Definition(
        DefType.Str,
        format = Format.Email,
        defaultValue = Some(str("user@example.com")),
        deprecated = true
      )
      val json = original.json
      val restored = json.as[Definition]
      restored.format should be(Format.Email)
      restored.defaultValue should be(Some(str("user@example.com")))
      restored.deprecated should be(true)
    }
    "serialize Format.Raw as absent (not included in JSON)" in {
      val d = Definition(DefType.Str)
      val json = d.json
      json.asObj.get("format") should be(None)
    }
    "handle RW[GenericType] round-trip" in {
      val gt = GenericType("T", Definition(DefType.Str))
      val json = gt.json
      val restored = json.as[GenericType]
      restored.name should be("T")
      restored.definition.defType should be(DefType.Str)
    }
    "handle recursive/self-referencing types" in {
      import RecursiveTest._
      val tree = TreeNode("root", List(
        TreeNode("child1", Nil),
        TreeNode("child2", List(TreeNode("grandchild", Nil)))
      ))
      val json = tree.json
      json("value").asString should be("root")
      json("children").asArr.value should have size 2
      json("children").asArr.value(1)("children").asArr.value(0)("value").asString should be("grandchild")

      val back = json.as[TreeNode]
      back should be(tree)

      // Option-based recursion
      val linked = LinkedNode(1, Some(LinkedNode(2, Some(LinkedNode(3, None)))))
      val linkedJson = linked.json
      linkedJson("value").asInt should be(1)
      val linkedBack = linkedJson.as[LinkedNode]
      linkedBack should be(linked)
    }
    "extract @description annotations into DefType definitions" in {
      import DescriptionTest._
      val defn = implicitly[RW[Documented]].definition
      defn.defType match {
        case DefType.Obj(map) =>
          map("name").description should be(Some("The person's full name"))
          map("age").description should be(Some("Age in years"))
        case other => fail(s"Expected DefType.Obj, got: $other")
      }
    }
  }

  enum Color {
    case Red, Green, Blue
  }

  object Color {
    implicit val rw: RW[Color] = RW.genEnum
  }

  enum Direction {
    case North, South, East, West
  }
  object Direction {
    given RW[Direction] = RW.gen
  }
}

object CustomFieldTest {
  @fabric.rw.typeField("kind")
  sealed trait CustomShape derives RW
  object CustomShape {
    case class Round(radius: Double) extends CustomShape derives RW
    case class Square(side: Double) extends CustomShape derives RW
  }
}

object ParameterizedEnumTest {
  enum Shape derives RW {
    case Circle(radius: Double)
    case Rectangle(width: Double, height: Double)
  }
}

object ErrorTest {
  case class Inner(value: Int) derives RW
  case class Outer(name: String, inner: Inner) derives RW
  case class Simple(x: Int) derives RW
}

object MixedEnumTest {
  enum Shape derives RW {
    case Circle(radius: Double)
    case Rectangle(width: Double, height: Double)
    case Point
  }
}

object DescriptionTest {
  case class Documented(@description("The person's full name") name: String, @description("Age in years") age: Int) derives RW
}

object SerializedTest {
  case class NamedPerson(firstName: String, lastName: String) derives RW {
    @serialized def fullName: String = s"$firstName $lastName"
  }
  case class WithCustomKey(value: Int) derives RW {
    @serialized("computed_double") def doubled: Int = value * 2
  }
}

object TransientTest {
  case class Config(name: String, @notSerialized secret: String = "default") derives RW
}

object AnyValTest {
  case class UserId(value: String) extends AnyVal
  object UserId {
    given RW[UserId] = RW.gen
  }
}

object RecursiveTest {
  case class TreeNode(value: String, children: List[TreeNode]) derives RW
  case class LinkedNode(value: Int, next: Option[LinkedNode]) derives RW
  case class Foo(name: String, bar: Bar) derives RW
  case class Bar(value: Int, foo: Option[Foo]) derives RW
}

object UnionTest {
  case class Cat(name: String, age: Int) derives RW
  case class Dog(name: String, breed: String) derives RW
  case class Fish(name: String, saltwater: Boolean) derives RW
  given catOrDogRW: RW[Cat | Dog] = RW.gen[Cat | Dog]
  given catOrDogOrFishRW: RW[Cat | Dog | Fish] = RW.gen[Cat | Dog | Fish]
}

object GenericTypeTest {
  case class Wrapper[T](value: T)
  object Wrapper {
    def rw[T: RW]: RW[Wrapper[T]] = RW.gen
  }

  case class Pair[A, B](first: A, second: B)
  object Pair {
    def rw[A: RW, B: RW]: RW[Pair[A, B]] = RW.gen
  }
}

object FormatTest {
  case class Contact(
    name: String,
    @format(Format.Email) email: String,
    @format(Format.Uri) website: String
  )
  object Contact {
    given rw: RW[Contact] = RW.gen
  }
}

object DeprecatedTest {
  case class ApiResponse(
    newField: String,
    @fieldDeprecated oldField: String
  )
  object ApiResponse {
    given rw: RW[ApiResponse] = RW.gen
  }
}

object DefaultTest {
  case class Config(
    name: String,
    host: String = "localhost",
    port: Int = 8080
  )
  object Config {
    given rw: RW[Config] = RW.gen
  }
}

case class Id[T](value: String) derives RW

object GenericCollisionTest {
  case class Box[T](value: T)
  object Box {
    def rw[T: RW]: RW[Box[T]] = RW.gen
  }
}