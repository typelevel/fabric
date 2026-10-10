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
import fabric.define.DefType
import fabric.dsl._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnknownCaseSpec extends AnyWordSpec with Matchers {
  import UnknownCaseTest._

  "@unknownCase on an enum" should {
    "decode a removed case into the marked case, keeping its discriminator" in {
      obj("type" -> "UnknownCaseTest.Notice.Expired", "since" -> 3).as[Notice] should be(
        Notice.Retired("UnknownCaseTest.Notice.Expired")
      )
    }
    "decode a removed parameterless case" in {
      obj("type" -> "UnknownCaseTest.Notice.Muted").as[Notice] should be(Notice.Retired("UnknownCaseTest.Notice.Muted"))
    }
    "still decode and encode every declared case" in {
      val opened: Notice = Notice.Opened(42L)
      opened.json should be(obj("type" -> "UnknownCaseTest.Notice.Opened", "at" -> 42))
      opened.json.as[Notice] should be(opened)
      val closed: Notice = Notice.Closed
      closed.json.as[Notice] should be(closed)
    }
    "write the marked case under the discriminator it was read from" in {
      val retired: Notice = Notice.Retired("UnknownCaseTest.Notice.Expired")
      retired.json should be(obj("type" -> "UnknownCaseTest.Notice.Expired"))
      retired.json.as[Notice] should be(retired)
    }
    "decode a removed case nested inside a stored record" in {
      obj(
        "id" -> "n1",
        "notice" -> obj("type" -> "UnknownCaseTest.Notice.Expired", "since" -> 3)
      ).as[Stored] should be(Stored("n1", Notice.Retired("UnknownCaseTest.Notice.Expired")))
    }
    "leave the marked case out of the definition" in {
      summon[RW[Notice]].definition.defType match {
        case DefType.Poly(values, _) =>
          values.keySet should be(Set("UnknownCaseTest.Notice.Opened", "UnknownCaseTest.Notice.Closed"))
        case other => fail(s"Expected a Poly definition, got $other")
      }
    }
    "still refuse a value that is not an object or has no discriminator" in {
      an[RWException] should be thrownBy str("oops").as[Notice]
      an[RWException] should be thrownBy obj("at" -> 1).as[Notice]
    }
  }

  "@unknownCase on a sealed trait" should {
    "decode an unknown discriminator under a custom type field" in {
      obj("kind" -> "UnknownCaseTest.Event.Removed", "x" -> 1).as[Event] should be(
        Event.Unknown("UnknownCaseTest.Event.Removed")
      )
      val started: Event = Event.Started("a")
      started.json should be(obj("kind" -> "UnknownCaseTest.Event.Started", "name" -> "a"))
      started.json.as[Event] should be(started)
      (Event.Unknown("Gone"): Event).json should be(obj("kind" -> "Gone"))
    }
  }

  "@unknownCase on an enum written as strings" should {
    "write every declared case as the same plain string as an enum without the marked case" in {
      (Level.Low: Level).json should be(str("UnknownCaseTest.Level.Low"))
      (Level.High: Level).json should be(str("UnknownCaseTest.Level.High"))
      (PlainLevel.High: PlainLevel).json should be(str("UnknownCaseTest.PlainLevel.High"))
    }
    "read every declared case by its full name and by its leaf name" in {
      str("UnknownCaseTest.Level.High").as[Level] should be(Level.High)
      str("high").as[Level] should be(Level.High)
    }
    "decode an unknown string into the marked case" in {
      str("UnknownCaseTest.Level.Medium").as[Level] should be(Level.Retired("UnknownCaseTest.Level.Medium"))
      str("Medium").as[Level] should be(Level.Retired("Medium"))
    }
    "write the marked case back as the same plain string" in {
      val retired: Level = Level.Retired("UnknownCaseTest.Level.Medium")
      retired.json should be(str("UnknownCaseTest.Level.Medium"))
      retired.json.as[Level] should be(retired)
    }
    "decode an unknown string nested inside a stored record" in {
      obj("name" -> "a", "level" -> "Medium").as[Leveled] should be(Leveled("a", Level.Retired("Medium")))
      Leveled("a", Level.Retired("Medium")).json should be(obj("name" -> "a", "level" -> "Medium"))
    }
    "keep the definition of an enum without the marked case" in {
      val marked = summon[RW[Level]].definition
      val plain = summon[RW[PlainLevel]].definition
      (marked.defType, plain.defType) match {
        case (DefType.Poly(values, _), DefType.Poly(plainValues, _)) =>
          values.keySet should be(Set("UnknownCaseTest.Level.Low", "UnknownCaseTest.Level.High"))
          values.keySet.map(_.replace(".Level.", ".PlainLevel.")) should be(plainValues.keySet)
          values.values.toList should be(plainValues.values.toList)
        case other => fail(s"Expected Poly definitions, got $other")
      }
      marked.className should be(plain.className.map(_.replace("PlainLevel", "Level")))
    }
    "still refuse a value that is not a string" in {
      an[RWException] should be thrownBy obj("type" -> "UnknownCaseTest.Level.Low").as[Level]
      an[RWException] should be thrownBy num(1).as[Level]
    }
  }

  "@unknownCase on a sealed trait of case objects" should {
    "write every case object as the string it had without the marked case, and take any other string" in {
      (Color.Red: Color).json should be(str("UnknownCaseTest.Color.Red"))
      (PlainColor.Red: PlainColor).json should be(str("UnknownCaseTest.PlainColor.Red"))
      str("UnknownCaseTest.Color.Red").as[Color] should be(Color.Red)
      str("UnknownCaseTest.Color.Green").as[Color] should be(Color.Other("UnknownCaseTest.Color.Green"))
      (Color.Other("UnknownCaseTest.Color.Green"): Color).json should be(str("UnknownCaseTest.Color.Green"))
      summon[RW[Color]].definition.defType match {
        case DefType.Poly(values, _) => values.keySet should be(Set("UnknownCaseTest.Color.Red", "UnknownCaseTest.Color.Blue"))
        case other                   => fail(s"Expected a Poly definition, got $other")
      }
    }
  }

  "a derivation without @unknownCase" should {
    "still refuse an unknown discriminator" in {
      an[RWException] should be thrownBy obj("type" -> "UnknownCaseTest.Plain.Gone").as[Plain]
    }
  }

  "@unknownCase on a case of the wrong shape" should {
    "fail to derive" in {
      assertDoesNotCompile("""
        enum TwoFields derives fabric.rw.RW {
          case A(x: Int)
          @fabric.rw.unknownCase case Other(name: String, extra: Int)
        }
      """)
      assertDoesNotCompile("""
        enum NotAString derives fabric.rw.RW {
          case A(x: Int)
          @fabric.rw.unknownCase case Other(name: Int)
        }
      """)
      assertDoesNotCompile("""
        enum TwoMarked derives fabric.rw.RW {
          case A(x: Int)
          @fabric.rw.unknownCase case Other(name: String)
          @fabric.rw.unknownCase case Another(name: String)
        }
      """)
    }
  }
}

object UnknownCaseTest {
  enum Notice derives RW {
    case Opened(at: Long)
    case Closed
    @unknownCase case Retired(wireName: String)
  }

  case class Stored(id: String, notice: Notice) derives RW

  @typeField("kind")
  sealed trait Event derives RW
  object Event {
    case class Started(name: String) extends Event derives RW
    @unknownCase case class Unknown(wireName: String) extends Event derives RW
  }

  enum Plain derives RW {
    case Kept(x: Int)
  }

  enum Level derives RW {
    case Low
    case High
    @unknownCase case Retired(wireName: String)
  }

  enum PlainLevel derives RW {
    case Low
    case High
  }

  case class Leveled(name: String, level: Level) derives RW

  sealed trait Color derives RW
  object Color {
    case object Red extends Color
    case object Blue extends Color
    @unknownCase case class Other(wireName: String) extends Color
  }

  sealed trait PlainColor derives RW
  object PlainColor {
    case object Red extends PlainColor
    case object Blue extends PlainColor
  }
}
