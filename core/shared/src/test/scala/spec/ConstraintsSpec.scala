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
import fabric.rw._

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConstraintsSpec extends AnyWordSpec with Matchers {
  "Constraints" should {
    "report isEmpty when all fields are None" in {
      Constraints.Empty.isEmpty should be(true)
      Constraints().isEmpty should be(true)
      Constraints(pattern = Some("^x")).isEmpty should be(false)
    }
    "attach via Definition.applyFieldConstraints" in {
      val base = Definition(
        DefType.Obj(
          scala.collection.immutable.VectorMap(
            "name" -> Definition(DefType.Str),
            "age" -> Definition(DefType.Int)
          )
        )
      )
      val constrained = Definition.applyFieldConstraints(
        base,
        Map(
          "name" -> Constraints(pattern = Some("^[A-Z]"), minLength = Some(1)),
          "age" -> Constraints(minimum = Some(0.0), maximum = Some(120.0))
        )
      )
      val fields = constrained.defType.asInstanceOf[DefType.Obj].map
      fields("name").constraints.pattern should be(Some("^[A-Z]"))
      fields("name").constraints.minLength should be(Some(1))
      fields("age").constraints.minimum should be(Some(0.0))
      fields("age").constraints.maximum should be(Some(120.0))
    }
    "serialize and deserialize all constraint attributes" in {
      val d = Definition(
        DefType.Str,
        constraints = Constraints(
          pattern = Some("^[a-z]+$"),
          minLength = Some(3),
          maxLength = Some(12),
          minimum = Some(1.5),
          maximum = Some(9.5),
          exclusiveMinimum = Some(0.0),
          exclusiveMaximum = Some(10.0),
          multipleOf = Some(0.5),
          minItems = Some(1),
          maxItems = Some(100),
          uniqueItems = Some(true)
        )
      )
      val json = d.json
      json("pattern").asString should be("^[a-z]+$")
      json("minLength").asInt should be(3)
      json("uniqueItems").asBool.value should be(true)
      val restored = json.as[Definition]
      restored.constraints should be(d.constraints)
    }
    "omit constraint keys from JSON when empty" in {
      val d = Definition(DefType.Str)
      val json = d.json
      json.asObj.get("pattern") should be(None)
      json.asObj.get("minLength") should be(None)
      json.asObj.get("uniqueItems") should be(None)
    }
  }

  "RW-derived constraints" should {
    "populate from @pattern / @minLength / @maxLength annotations" in {
      val d = ConstrainedUser.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("username").constraints.pattern should be(Some("^[a-z][a-z0-9_]*$"))
      fields("username").constraints.minLength should be(Some(3))
      fields("username").constraints.maxLength should be(Some(20))
      fields("name").constraints.isEmpty should be(true)
    }
    "populate from @minimum / @maximum / @multipleOf" in {
      val d = ConstrainedUser.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("age").constraints.minimum should be(Some(0.0))
      fields("age").constraints.maximum should be(Some(120.0))
      fields("score").constraints.multipleOf should be(Some(0.5))
    }
    "populate from @minItems / @maxItems / @uniqueItems" in {
      val d = ConstrainedUser.rw.definition
      val fields = d.defType.asInstanceOf[DefType.Obj].map
      fields("tags").constraints.minItems should be(Some(1))
      fields("tags").constraints.maxItems should be(Some(10))
      fields("tags").constraints.uniqueItems should be(Some(true))
    }
  }
}

case class ConstrainedUser(
  name: String,
  @pattern("^[a-z][a-z0-9_]*$") @minLength(3) @maxLength(20) username: String,
  @minimum(0.0) @maximum(120.0) age: Int,
  @multipleOf(0.5) score: Double,
  @minItems(1) @maxItems(10) @uniqueItems tags: List[String]
)

object ConstrainedUser {
  implicit val rw: RW[ConstrainedUser] = RW.gen
}
