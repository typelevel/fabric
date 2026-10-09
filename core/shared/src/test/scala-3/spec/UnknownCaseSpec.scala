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
}
