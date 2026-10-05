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

/** NestedInitSpec for a class whose RW is its `derives RW` given. */
class NestedDerivesInitSpec extends AnyWordSpec with Matchers {
  import NestedDerivesInitSpec._

  "A derived given RW of a class nested in an object registering it" should {
    "be made first from another object's initializer" in {
      val rw = Initializes(First.rw)
      Outer.registered should have size 3
      rw.write(obj("message" -> "m", "percent" -> 0.5)) should be(Outer.Progress("m", Some(0.5)))
    }
  }
}

object NestedDerivesInitSpec {
  trait Output

  object Outer {
    case object Pending extends Output
    case class Progress(message: String, percent: Option[Double] = None) extends Output derives RW
    case class Done(note: Option[String]) extends Output derives RW
    val registered: List[RW[?]] = List(RW.static(Pending), summon[RW[Progress]], summon[RW[Done]])
  }
  object First {
    val rw: RW[Outer.Progress] = summon[RW[Outer.Progress]]
  }
}
