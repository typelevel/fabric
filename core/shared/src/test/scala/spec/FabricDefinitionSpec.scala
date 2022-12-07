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

import fabric.define.{DefType, FabricDefinition, FabricGenerator}
import fabric.{define, _}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap

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
      define.FabricDefinition(
        obj(
          "name" -> "John Doe",
          "age" -> 50
        )
      ) should be(
        DefType.Obj(
          ListMap(
            "name" -> DefType.Str,
            "age" -> DefType.Int
          )
        )
      )
    }
    "represent a simple obj with optional value" in {
      FabricDefinition(
        List(
          obj(
            "name" -> "John Doe",
            "age" -> 50
          ),
          obj(
            "name" -> "Jane Doe"
          )
        )
      ) should be(
        DefType.Obj(
          ListMap(
            "name" -> DefType.Str,
            "age" -> DefType.Opt(DefType.Int)
          )
        )
      )
    }
    "represent a simple optional obj" in {
      val d = FabricDefinition(
        List(
          Null,
          obj(
            "name" -> "Jane Doe",
            "age" -> 50
          )
        )
      )
      d should be(
        DefType.Opt(
          DefType.Obj(
            ListMap(
              "name" -> DefType.Str,
              "age" -> DefType.Int
            )
          )
        )
      )
    }
    "fail with conflicting types" in {
      assertThrows[RuntimeException](
        FabricDefinition(
          List(
            obj("name" -> "Bad"),
            num(5)
          )
        )
      )
    }
    "validate a definition" in {
      val definition = DefType.Obj(
        ListMap(
          "name" -> DefType.Str,
          "age" -> DefType.Opt(DefType.Int)
        )
      )
      val value = obj(
        "name" -> "Jane Doe",
        "age" -> 50
      )
      definition.validate(value) should be(true)
    }
    "fail to validate a definition" in {
      val definition = DefType.Obj(
        ListMap(
          "name" -> DefType.Str,
          "age" -> DefType.Int
        )
      )
      val value = obj(
        "name" -> "Jane Doe"
      )
      definition.validate(value) should be(false)
    }
    "generate a case class based on a definition" in {
      val definition = DefType.Obj(
        ListMap(
          "name" -> DefType.Str,
          "age" -> DefType.Int
        )
      )
      val generated =
        FabricGenerator.withMappings(definition, "com.example.Person")
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be("""package com.example
          |
          |import fabric.rw._
          |
          |case class Person(name: String, age: Long)
          |
          |object Person {
          |  implicit val rw: RW[Person] = RW
          |}""".stripMargin)
      generated.additional should be(Nil)
    }
    "generate two case classes based on a definition" in {
      val definition = DefType.Obj(
        ListMap(
          "name" -> DefType.Str,
          "age" -> DefType.Int,
          "location" -> DefType.Obj(
            ListMap(
              "city" -> DefType.Str,
              "state" -> DefType.Str
            )
          )
        )
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
          |case class Person(name: String, age: Long, location: com.example.Location)
          |
          |object Person {
          |  implicit val rw: RW[Person] = RW
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
                                |  implicit val rw: RW[Location] = RW
                                |}""".stripMargin)
    }
    "generate two case classes based on a definition with an array" in {
      val definition = DefType.Obj(
        ListMap(
          "name" -> DefType.Str,
          "age" -> DefType.Int,
          "locations" -> DefType.Arr(
            DefType.Obj(
              ListMap(
                "city" -> DefType.Str,
                "state" -> DefType.Str
              )
            )
          )
        )
      )
      val generated = FabricGenerator.withMappings(
        definition,
        "com.example.Person",
        "locations" -> "com.example.Location"
      )
      generated.packageName should be(Some("com.example"))
      generated.className should be("Person")
      generated.code should be("""package com.example
          |
          |import fabric.rw._
          |
          |case class Person(name: String, age: Long, locations: Vector[com.example.Location])
          |
          |object Person {
          |  implicit val rw: RW[Person] = RW
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
          |  implicit val rw: RW[Location] = RW
          |}""".stripMargin)
    }
    /*"generate complex case classes from large JSON" in {
      val json: List[Json] = JsonParser(
        Source.fromInputStream(
          getClass.getClassLoader.getResourceAsStream("large.json")
        ),
        Format.Json
      ).asVector.toList
      val dt = FabricDefinition(json)
      val generated = FabricGenerator(
        dt = dt,
        rootName = "bench.event.Event",
        (key: String) => {
          val name = "_(.)".r
            .replaceAllIn(
              key,
              m => {
                m.group(1).toUpperCase
              }
            )
            .capitalize
          s"bench.event.Event$name"
        }
      )
      generated.write(new File("bench/src/main/scala/"))
    }*/
  }
}
