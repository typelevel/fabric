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

package fabric.react

import fabric.{obj, Json}
import fabric.rw._
import reactify._

trait ReactParent {
  private val _json: Var[Json] = Var(load().getOrElse(obj()))
  private val list: Var[List[RWVar[_]]] = Var(Nil)

  def json: Val[Json] = _json

  protected def modify(f: Json => Json): Unit = synchronized {
    val modified = f(json())
    _json @= modified
  }

  protected def reactive[T: RW](name: String, default: => T): Var[T] = synchronized {
    val rwv = RWVar[T](name, () => default)
    list @= rwv :: list()
    rwv.v
  }

  protected def load(): Option[Json]

  case class RWVar[T](name: String, default: () => T)(implicit rw: RW[T]) {
    val v: Var[T] = Var(load())

    v.attachAndFire { value =>
      modify { json =>
        json.merge(obj(name -> value.asJson))
      }
    }

    protected def load(): T = json().get(name) match {
      case Some(j) => j.as[T]
      case None => default()
    }
  }
}
