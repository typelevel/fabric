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

import fabric.define._
import fabric.{define, _}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FabricDefinitionSpec extends AnyWordSpec with Matchers {
  "FabricDefinition" should {
    "represent an Int properly" in {
      FabricDefinition(num(5)) should be(DefType.Int)
    }
    "represent a Null properly" in {
      define.FabricDefinition(Null) should be(DefType.Null)
    }
    "represent an optional Int properly" in {
      FabricDefinition(List(num(5), Null)) should be(DefType.Opt(DefType.Int))
    }
    "represent an optional Int properly starting with Null" in {
      FabricDefinition(List(Null, num(5))) should be(DefType.Opt(DefType.Int))
    }
    "represent a simple obj" in {
      define.FabricDefinition(obj("name" -> "John Doe", "age" -> 50)) should be(
        DefType.Obj("name" -> DefType.Str, "age" -> DefType.Int)
      )
    }
    "represent a simple obj with optional value" in {
      FabricDefinition(
        List(obj("name" -> "John Doe", "age" -> 50), obj("name" -> "Jane Doe"))
      ) should be(
        DefType.Obj("name" -> DefType.Str, "age" -> DefType.Opt(DefType.Int))
      )
    }
    "represent a simple optional obj" in {
      val d = FabricDefinition(List(Null, obj("name" -> "Jane Doe", "age" -> 50)))
      d should be(
        DefType.Opt(DefType.Obj("name" -> DefType.Str, "age" -> DefType.Int))
      )
    }
    "represent null lists" in {
      val d = FabricDefinition(
        List(
          obj(),
          obj("list" -> arr(obj("name" -> "Test"))),
          obj("list" -> Null)
        )
      )
      d should be(
        DefType.Obj("list" -> DefType.Arr(DefType.Obj("name" -> DefType.Str)))
      )
    }
    "represent multiple numeric types" in {
      DefType.Dec.merge(DefType.Int) should be(DefType.Dec)
      DefType.Int.merge(DefType.Dec) should be(DefType.Dec)
    }
    "fail with conflicting types" in {
      assertThrows[RuntimeException](
        FabricDefinition(List(obj("name" -> "Bad"), num(5)))
      )
    }
    "validate a definition" in {
      val definition = DefType.Obj("name" -> DefType.Str, "age" -> DefType.Opt(DefType.Int))
      val value = obj("name" -> "Jane Doe", "age" -> 50)
      definition.validate(value) should be(true)
    }
    "fail to validate a definition" in {
      val definition = DefType.Obj("name" -> DefType.Str, "age" -> DefType.Int)
      val value = obj("name" -> "Jane Doe")
      definition.validate(value) should be(false)
    }
    "generate a case class based on a definition" in {
      val definition = DefType.Obj("name" -> DefType.Str, "age" -> DefType.Int)
      val generated = FabricGenerator.withMappings(definition, "com.example.Person")
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be("""|package com.example
                                  |
                                  |import fabric.rw._
                                  |
                                  |case class Person(name: String,
                                  |                  age: Long)
                                  |
                                  |object Person {
                                  |  implicit val rw: RW[Person] = RW.gen
                                  |}""".stripMargin)
      generated.additional should be(Nil)
    }
    "generate two case classes based on a definition" in {
      val definition = DefType.Obj(
        "name" -> DefType.Str,
        "age" -> DefType.Int,
        "location" -> DefType.Obj("city" -> DefType.Str, "state" -> DefType.Str)
      )
      val generated = FabricGenerator.withMappings(
        definition,
        "com.example.Person",
        "location" -> "com.example.Location"
      )
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be("""package com.example
                                 |
                                 |import fabric.rw._
                                 |
                                 |case class Person(name: String,
                                 |                  age: Long,
                                 |                  location: Location)
                                 |
                                 |object Person {
                                 |  implicit val rw: RW[Person] = RW.gen
                                 |}""".stripMargin)
      generated.additional.length should be(1)
      val location = generated.additional.head
      location.packageName should be(Some("com.example"))
      location.className should be("Location")
      location.code should be("""package com.example
                                |
                                |import fabric.rw._
                                |
                                |case class Location(city: String,
                                |                    state: String)
                                |
                                |object Location {
                                |  implicit val rw: RW[Location] = RW.gen
                                |}""".stripMargin)
    }
    "generate two case classes based on a definition with an array" in {
      val definition = DefType.Obj(
        "name" -> DefType.Str,
        "age" -> DefType.Int,
        "locations" -> DefType.Arr(
          DefType.Obj("city" -> DefType.Str, "state" -> DefType.Str)
        )
      )
      val generated = FabricGenerator(
        dt = definition,
        rootName = "com.example.Person",
        resolver = (key: String) => {
          val name =
            if (key.endsWith("s")) {
              key.substring(0, key.length - 1)
            } else {
              key
            }
          s"com.example.${name.capitalize}"
        },
        extras = (className: String) =>
          ClassExtras(
            fields = List(ClassField("id", "Int", Some("-1"))) ::: (if (className == "com.example.Location") {
                                                                      List(
                                                                        ClassField(
                                                                          "state",
                                                                          "String",
                                                                          Some(
                                                                            "\"Unknown\""
                                                                          )
                                                                        )
                                                                      )
                                                                    } else Nil),
            bodyContent = Some("  // Extra content")
          )
      )
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be("""package com.example
                                 |
                                 |import fabric.rw._
                                 |
                                 |case class Person(name: String,
                                 |                  age: Long,
                                 |                  locations: Vector[Location],
                                 |                  id: Int = -1)
                                 |
                                 |object Person {
                                 |  implicit val rw: RW[Person] = RW.gen
                                 |
                                 |  // Extra content
                                 |}""".stripMargin)
      generated.additional.length should be(1)
      val location = generated.additional.head
      location.packageName should be(Some("com.example"))
      location.className should be("Location")
      location.code should be("""package com.example
                                |
                                |import fabric.rw._
                                |
                                |case class Location(city: String,
                                |                    state: String = "Unknown",
                                |                    id: Int = -1)
                                |
                                |object Location {
                                |  implicit val rw: RW[Location] = RW.gen
                                |
                                |  // Extra content
                                |}""".stripMargin)
    }
  }
}
