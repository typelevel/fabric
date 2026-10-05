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

import java.util.concurrent.atomic.AtomicReference

/**
  * A class's derived RW is made without initializing the object the class is nested in, so an object registering the
  * RWs of its nested classes in its own initializer can be reached first through one of those RWs.
  */
class NestedInitSpec extends AnyWordSpec with Matchers {
  import NestedInitSpec._

  "A derived RW of a class nested in an object registering it" should {
    "be made first from another object's initializer for a field with a default" in {
      val rw = Initializes(DefaultFirst.rw)
      DefaultOuter.registered should have size 2
      rw.write(obj("message" -> "m")) should be(DefaultOuter.Progress("m"))
    }
    "be made first from another object's initializer for an Option field" in {
      val rw = Initializes(OptionFirst.rw)
      OptionOuter.registered should have size 1
      rw.write(obj("text" -> "t")) should be(OptionOuter.Note("t", None))
    }
    "be made first from another object's initializer for a generic class with a default" in {
      val rw = Initializes(GenericFirst.rw)
      GenericOuter.registered should have size 1
      rw.write(obj("value" -> 3)) should be(GenericOuter.Box(3))
    }
    "be made first from another object's initializer for a class at the top of its package" in {
      val rw = Initializes(TopFirst.rw)
      TopRegistry.registered should have size 1
      rw.write(obj("name" -> "n")) should be(NestedTop("n"))
    }
  }
}

object NestedInitSpec {
  trait Output

  object DefaultOuter {
    case object Pending extends Output
    case class Progress(message: String, percent: Option[Double] = None, step: Int = 1) extends Output
    object Progress {
      implicit lazy val rw: RW[Progress] = RW.gen[Progress]
    }
    val registered: List[RW[_]] = List(RW.static(Pending), implicitly[RW[Progress]])
  }
  object DefaultFirst {
    val rw: RW[DefaultOuter.Progress] = implicitly[RW[DefaultOuter.Progress]]
  }

  object OptionOuter {
    case class Note(text: String, tag: Option[String]) extends Output
    object Note {
      implicit lazy val rw: RW[Note] = RW.gen[Note]
    }
    val registered: List[RW[_]] = List(implicitly[RW[Note]])
  }
  object OptionFirst {
    val rw: RW[OptionOuter.Note] = implicitly[RW[OptionOuter.Note]]
  }

  object GenericOuter {
    case class Box[A](value: A, label: String = "box") extends Output
    object Box {
      implicit lazy val intRW: RW[Box[Int]] = RW.gen[Box[Int]]
    }
    val registered: List[RW[_]] = List(Box.intRW)
  }
  object GenericFirst {
    val rw: RW[GenericOuter.Box[Int]] = GenericOuter.Box.intRW
  }

  object TopRegistry {
    val registered: List[RW[_]] = List(implicitly[RW[NestedTop]])
  }
  object TopFirst {
    val rw: RW[NestedTop] = implicitly[RW[NestedTop]]
  }
}

case class NestedTop(name: String, count: Int = 0, note: Option[String] = None)

object NestedTop {
  implicit lazy val rw: RW[NestedTop] = RW.gen[NestedTop]
}

/**
  * A value initialized on a thread of its own, failing where it does not finish within ten seconds.
  */
object Initializes {
  def apply[T](value: => T): T = {
    val result = new AtomicReference[Either[Throwable, T]]
    val thread = new Thread(new Runnable {
      override def run(): Unit = result.set(try Right(value)
      catch { case t: Throwable => Left(t) })
    })
    thread.setDaemon(true)
    thread.start()
    thread.join(10000L)
    Option(result.get()) match {
      case Some(Right(t)) => t
      case Some(Left(t)) => throw t
      case None =>
        val trace = thread.getStackTrace.mkString("\n")
        thread.interrupt()
        throw new AssertionError(s"initialization did not finish:\n$trace")
    }
  }
}
