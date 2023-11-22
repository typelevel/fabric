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

package fabric.transform

import fabric._
import fabric.define.DefType
import fabric.rw._

import scala.util.matching.Regex

class Transformer(val json: Json, val paths: List[JsonPath]) {
  def modify(modifier: Json => Json): Json = paths.foldLeft(json)((json, path) => json.modify(path)(modifier))

  def move(to: JsonPath = JsonPath.empty): Json = paths.foldLeft(json) { (json, path) =>
    val value = json(path)
    json.remove(path).modify(to)(_.merge(value))
  }

  def copy(to: JsonPath = JsonPath.empty): Json = paths.foldLeft(json) { (json, path) =>
    val value = json(path)
    json.modify(to)(_.merge(value))
  }

  def rename(newName: String): Json = paths.foldLeft(json) { (json, path) =>
    val prePath = JsonPath(path.entries.init)
    json.modify(prePath) { value =>
      val p = JsonPath(path.entries.last)
      value.remove(p).merge(obj(newName -> value(p)))
    }
  }

  def mergeTo(path: JsonPath): Json = paths.foldLeft(json) { (j, p) =>
    val value = j(p)
    j.modify(path) { destination =>
      destination.merge(value)
    }
  }

  def concatenate(path: JsonPath, separator: String = ""): Json = paths.foldLeft(json) { (j, p) =>
    val value = j(p)
    j.modify(path) { destination =>
      if (destination.isStr) {
        s"${destination.asString}$separator${value.asString}"
      } else {
        value.asString
      }
    }
  }

  def extract(regex: Regex, to: Vector[JsonPath]): Json = paths.foldLeft(json) { (j, p) =>
    val value = j(p).asString
    val matcher = regex.pattern.matcher(value)
    matcher.matches()
    (1 to matcher.groupCount()).foldLeft(j)((json, group) => json.modify(to(group - 1))(_ => matcher.group(group)))
  }

  def delete(): Json = modify(_ => obj())
}

object Transformer {
  implicit val rw: RW[Transformer] = RW.from(
    r = t => obj("json" -> t.json, "paths" -> t.paths.json),
    w = j => new Transformer(j("json"), j("paths").as[List[JsonPath]]),
    d = DefType
      .Obj(Some("fabric.transform.Transformer"), "json" -> DefType.Json, "paths" -> DefType.Arr(JsonPath.rw.definition))
  )
}
