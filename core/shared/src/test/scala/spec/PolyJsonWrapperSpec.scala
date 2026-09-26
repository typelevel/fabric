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
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PolyJsonWrapperSpec extends AnyWordSpec with Matchers {
  import PolyJsonWrapperSpec._

  "A JsonWrapper subtype of a polymorphic type" should {
    "read back without the discriminator inside its json" in {
      val payload = obj("query" -> str("weather"), "limit" -> num(3))
      val back = Payload(payload).asInstanceOf[Input].json.as[Input]
      back should be(Payload(payload))
      assert(back.asInstanceOf[Payload].json == payload)
    }
    "keep a field of the same name as the discriminator when the wrapped json has one" in {
      val payload = obj("type" -> str("celsius"), "value" -> num(21))
      assert(Payload(payload).asInstanceOf[Input].json.as[Input].asInstanceOf[Payload].json.get("value").contains(num(21)))
    }
  }

  "A plain subtype of a polymorphic type" should {
    "round-trip" in {
      (Named("a"): Input).json.as[Input] should be(Named("a"))
    }
  }
}

object PolyJsonWrapperSpec {
  sealed trait Input
  case class Payload(json: Json = Obj.empty) extends Input with JsonWrapper
  case class Named(name: String) extends Input

  object Payload {
    implicit val rw: RW[Payload] = RW.gen
  }
  object Named {
    implicit val rw: RW[Named] = RW.gen
  }
  implicit val inputRW: RW[Input] = RW.poly[Input]()(Payload.rw, Named.rw)
}
