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

import fabric.dsl.*
import fabric.define._
import fabric.define.Definition
import fabric.rw._
import fabric.{define, _}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FabricDefinitionSpec extends AnyWordSpec with Matchers {
  "FabricDefinition" should {
    "represent an Int properly" in {
      FabricDefinition(num(5)) should be(Definition(DefType.Int))
    }
    "represent a Null properly" in {
      define.FabricDefinition(Null) should be(Definition(DefType.Null))
    }
    "represent an optional Int properly" in {
      FabricDefinition(List(num(5), Null)) should be(Definition(DefType.Opt(Definition(DefType.Int))))
    }
    "represent an optional Int properly starting with Null" in {
      FabricDefinition(List(Null, num(5))) should be(Definition(DefType.Opt(Definition(DefType.Int))))
    }
    "represent a simple obj" in {
      define.FabricDefinition(obj("name" -> "John Doe", "age" -> 50)) should be(
        Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Int)))
      )
    }
    "represent a simple obj with optional value" in {
      FabricDefinition(
        List(obj("name" -> "John Doe", "age" -> 50), obj("name" -> "Jane Doe"))
      ) should be(
        Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Opt(Definition(DefType.Int)))))
      )
    }
    "represent a simple optional obj" in {
      val d = FabricDefinition(List(Null, obj("name" -> "Jane Doe", "age" -> 50)))
      d should be(
        Definition(DefType.Opt(Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Int)))))
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
        Definition(
          DefType.Obj(
            "list" -> Definition(
              DefType.Opt(Definition(DefType.Arr(Definition(DefType.Obj("name" -> Definition(DefType.Str))))))
            )
          )
        )
      )
    }
    "represent multiple numeric types" in {
      Definition(DefType.Dec).merge(Definition(DefType.Int)) should be(Definition(DefType.Dec))
      Definition(DefType.Int).merge(Definition(DefType.Dec)) should be(Definition(DefType.Dec))
    }
    "fail with conflicting types" in
      assertThrows[RuntimeException](
        FabricDefinition(List(obj("name" -> "Bad"), num(5)))
      )
    "validate a definition" in {
      val definition =
        Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Opt(Definition(DefType.Int)))))
      val value = obj("name" -> "Jane Doe", "age" -> 50)
      definition.validate(value) should be(true)
    }
    "fail to validate a definition" in {
      val definition = Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Int)))
      val value = obj("name" -> "Jane Doe")
      definition.validate(value) should be(false)
    }
    "generate a case class based on a definition" in {
      val definition = Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Int)))
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
      val definition = Definition(
        DefType.Obj(
          "name" -> Definition(DefType.Str),
          "age" -> Definition(DefType.Int),
          "location" -> Definition(DefType.Obj("city" -> Definition(DefType.Str), "state" -> Definition(DefType.Str)))
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
      val definition = Definition(
        DefType.Obj(
          "name" -> Definition(DefType.Str),
          "age" -> Definition(DefType.Int),
          "locations" -> Definition(
            DefType.Arr(
              Definition(DefType.Obj("city" -> Definition(DefType.Str), "state" -> Definition(DefType.Str)))
            )
          )
        )
      )
      val generated = FabricGenerator(
        definition = definition,
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
    "generate a schema for a Person" in {
      Person.rw.definition.json should be(
        obj(
          "type" -> "object",
          "values" -> obj(
            "name" -> obj("type" -> "string"),
            "age" -> obj("type" -> "numeric", "precision" -> "integer"),
            "address" -> obj(
              "type" -> "object",
              "values" -> obj(
                "city" -> obj("type" -> "string"),
                "state" -> obj("type" -> "string")
              ),
              "className" -> "spec.Address"
            )
          ),
          "className" -> "spec.Person"
        )
      )
    }
    "generate a template for a Person" in {
      Person.rw.definition.template(TemplateConfig.Empty) should be(
        obj(
          "name" -> "",
          "age" -> 0,
          "address" -> obj(
            "city" -> "",
            "state" -> ""
          )
        )
      )
    }
    "represent a default value on a non-optional Definition" in {
      User.rw.definition.json should be(
        obj(
          "type" -> "object",
          "values" -> obj(
            "name" -> obj(
              "type" -> "string",
              "default" -> "Unknown"
            ),
            "age" -> obj(
              "type" -> "numeric",
              "precision" -> "integer"
            )
          ),
          "className" -> "spec.User"
        )
      )
    }
    "represent a Map[String, String] properly" in {
      rw.mapRW[String, String].definition.json should be(
        obj(
          "type" -> "object",
          "values" -> obj(
            "[key]" -> obj(
              "type" -> "string"
            )
          )
        )
      )
    }
    "round-trip Definition for every DefType variant through JSON" in {
      val cases: List[Definition] = List(
        Definition(DefType.Str),
        Definition(DefType.Int),
        Definition(DefType.Dec),
        Definition(DefType.Bool),
        Definition(DefType.Json),
        Definition(DefType.Null),
        Definition(DefType.Arr(Definition(DefType.Str))),
        Definition(DefType.Opt(Definition(DefType.Int))),
        Definition(DefType.Obj("name" -> Definition(DefType.Str), "age" -> Definition(DefType.Int))),
        Definition(
          DefType.Poly(
            Map(
              "Red" -> Definition(DefType.Null),
              "Green" -> Definition(DefType.Null)
            )
          )
        )
      )
      cases.foreach { original =>
        val json = original.json
        val restored = json.as[Definition]
        restored should be(original)
      }
    }
    "round-trip Definition with all metadata through JSON" in {
      val original = Definition(
        DefType.Obj("value" -> Definition(DefType.Str, genericName = Some("T"))),
        className = Some("com.example.Wrapper"),
        description = Some("A wrapper type"),
        genericTypes = List(GenericType("T", Definition(DefType.Str)))
      )
      val json = original.json
      val restored = json.as[Definition]
      restored.defType should be(original.defType)
      restored.className should be(Some("com.example.Wrapper"))
      restored.description should be(Some("A wrapper type"))
      restored.genericTypes.length should be(1)
      restored.genericTypes.head.name should be("T")
      restored.genericTypes.head.definition.defType should be(DefType.Str)
      restored.defType.asInstanceOf[DefType.Obj].map("value").genericName should be(Some("T"))
    }
    "deserialize old enum JSON format as Poly" in {
      val oldEnumJson = obj(
        "type" -> "enum",
        "values" -> fabric.Arr(Vector(str("Car"), str("SUV"), str("Truck"))),
        "className" -> "spec.VehicleType"
      )
      val definition = oldEnumJson.as[Definition]
      definition.className should be(Some("spec.VehicleType"))
      definition.defType match {
        case DefType.Poly(values, _) =>
          values.keySet should be(Set("Car", "SUV", "Truck"))
          values.values.foreach(_.defType should be(DefType.Null))
        case other => fail(s"Expected DefType.Poly, got: $other")
      }
    }
    "apply genericNames to Definition fields" in {
      val d = Definition(
        DefType.Obj(
          "name" -> Definition(DefType.Str),
          "value" -> Definition(DefType.Str),
          "other" -> Definition(DefType.Opt(Definition(DefType.Str)))
        )
      )
      val result = Definition.applyGenericNames(d, Map("value" -> "T", "other" -> "T"))
      val fields = result.defType.asInstanceOf[DefType.Obj].map
      fields("name").genericName should be(None)
      fields("value").genericName should be(Some("T"))
      fields("other").genericName should be(Some("T"))
    }
    "not modify non-Obj Definition in applyGenericNames" in {
      val d = Definition(DefType.Str)
      val result = Definition.applyGenericNames(d, Map("value" -> "T"))
      result should be(d)
    }
  }
}
