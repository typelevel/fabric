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
import fabric.dsl._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CaseClassRWSpec extends AnyWordSpec with Matchers {
  import CaseClassRWSpec._

  "A derived case class RW" should {
    "write its fields in the constructor's order" in {
      Ordered(z = "z", a = 1, m = true).json.asObj.value.keys.toList should be(List("z", "a", "m"))
    }
    "run a field's default only where the field is missing or null, in the fields' order" in {
      Counter.calls = 0
      obj("name" -> "n", "id" -> "given", "other" -> "o").as[Stateful] should be(Stateful("given", "n", 7, "o"))
      Counter.calls should be(0)
      obj("name" -> "n", "other" -> Null).as[Stateful] should be(Stateful("id1", "n", 7, "id2"))
      Counter.calls should be(2)
    }
    "run no default to describe itself until one is asked for" in {
      Counter.calls = 0
      val fields = Stateful.rw.definition.defType.asInstanceOf[fabric.define.DefType.Obj].map
      Counter.calls should be(0)
      fields("other").defaultValue should be(Some(Str("id1")))
      Counter.calls should be(1)
    }
    "derive a generic class with defaults" in {
      given RW[Generic[Int]] = Generic.rw[Int]
      obj("value" -> 5).as[Generic[Int]] should be(Generic(5, Nil, "g"))
      Generic(5, List(6), "h").json.asObj.value - "_generic" should be(obj("value" -> 5, "more" -> arr(6), "label" -> "h").asObj.value)
      val fields = summon[RW[Generic[Int]]].definition.defType.asInstanceOf[fabric.define.DefType.Obj].map
      fields("label").defaultValue should be(Some(Str("g")))
    }
    "write a field with the Reader nearest the derivation and read it with its Writer" in {
      Loud(Upper("a")).json should be(obj("word" -> "<a>", "plain" -> "<d>"))
      obj("word" -> "b").as[Loud] should be(Loud(Upper("B")))
      val fields = Loud.rw.definition.defType.asInstanceOf[fabric.define.DefType.Obj].map
      fields("plain").defaultValue should be(Some(Str("<d>")))
    }
    "find a field in another case, take null as its default and fail on a required one missing" in {
      obj("NAME" -> "n", "N" -> Null).as[Stateful].n should be(7)
      val e = intercept[RWException](obj("id" -> "i").as[Stateful])
      e.getMessage should include("Stateful.name (and no defaults set) in {\"id\": \"i\"}")
    }
    "give a JsonWrapper's json the whole object where it is missing" in {
      obj("name" -> "n", "x" -> 1).as[Wrapped] should be(Wrapped("n", obj("name" -> "n", "x" -> 1)))
    }
    "derive a class of seventy fields without raising the inline limit" in {
      val wide = Wide()
      wide.json.asObj.value.size should be(70)
      wide.json.asObj.value.keys.head should be("f1")
      wide.json.asObj.value.keys.last should be("f70")
      wide.json.as[Wide] should be(wide)
      obj("f70" -> 7).as[Wide] should be(Wide(f70 = 7))
    }
  }
}

object CaseClassRWSpec {
  object Counter {
    var calls: Int = 0

    def next(): String = {
      calls += 1
      s"id$calls"
    }
  }

  case class Ordered(z: String, a: Int, m: Boolean)
  object Ordered {
    given rw: RW[Ordered] = RW.gen
  }

  case class Stateful(id: String = Counter.next(), name: String, n: Int = 7, other: String = Counter.next())
  object Stateful {
    given rw: RW[Stateful] = RW.gen
  }

  case class Generic[T](value: T, more: List[T] = Nil, label: String = "g")
  object Generic {
    def rw[T: RW]: RW[Generic[T]] = RW.gen
  }

  case class Upper(value: String)
  object Upper {
    given rw: RW[Upper] = RW.string(_.value, s => Upper(s.toUpperCase))
  }

  case class Loud(word: Upper, plain: Upper = Upper("d"))
  object Loud {
    // a Reader nearer the derivation than Upper's RW, which the derivation writes Loud's fields with
    given loudReader: Reader[Upper] = (u: Upper) => Str(s"<${u.value}>")
    given rw: RW[Loud] = RW.gen
  }

  case class Wrapped(name: String, json: Json) extends JsonWrapper
  object Wrapped {
    given rw: RW[Wrapped] = RW.gen
  }

  case class Wide(
    f1: Int = 1, f2: Int = 2, f3: Int = 3, f4: Int = 4, f5: Int = 5, f6: Int = 6, f7: Int = 7, f8: Int = 8, f9: Int = 9,
    f10: Int = 10, f11: Int = 11, f12: Int = 12, f13: Int = 13, f14: Int = 14, f15: Int = 15, f16: Int = 16,
    f17: Int = 17, f18: Int = 18, f19: Int = 19, f20: Int = 20, f21: Int = 21, f22: Int = 22, f23: Int = 23,
    f24: Int = 24, f25: Int = 25, f26: Int = 26, f27: Int = 27, f28: Int = 28, f29: Int = 29, f30: Int = 30,
    f31: Int = 31, f32: Int = 32, f33: Int = 33, f34: Int = 34, f35: Int = 35, f36: Int = 36, f37: Int = 37,
    f38: Int = 38, f39: Int = 39, f40: Int = 40, f41: Int = 41, f42: Int = 42, f43: Int = 43, f44: Int = 44,
    f45: Int = 45, f46: Int = 46, f47: Int = 47, f48: Int = 48, f49: Int = 49, f50: Int = 50, f51: Int = 51,
    f52: Int = 52, f53: Int = 53, f54: Int = 54, f55: Int = 55, f56: Int = 56, f57: Int = 57, f58: Int = 58,
    f59: Int = 59, f60: Int = 60, f61: Int = 61, f62: Int = 62, f63: Int = 63, f64: Int = 64, f65: Int = 65,
    f66: Int = 66, f67: Int = 67, f68: Int = 68, f69: Int = 69, f70: Int = 70
  )
  object Wide {
    given rw: RW[Wide] = RW.gen
  }
}
