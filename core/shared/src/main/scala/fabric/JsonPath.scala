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

package fabric

import fabric.define.DefType
import fabric.rw.RW

/**
  * Path is a convenience wrapper to represent paths for lookups or changes in
  * Json
  */
case class JsonPath(entries: List[JsonPathEntry]) extends AnyVal {

  /**
    * Convenience DSL to build paths
    */
  def \(entry: JsonPathEntry): JsonPath = new JsonPath(entries ::: List(entry))

  def \\(that: JsonPath): JsonPath = new JsonPath(entries ::: that.entries)

  def isEmpty: Boolean = entries.isEmpty

  def nonEmpty: Boolean = entries.nonEmpty

  /**
    * Retrieves the head path element
    */
  def apply(): JsonPathEntry = entries.head

  /**
    * Returns a new Path with the tail of this path
    */
  def next(): JsonPath = new JsonPath(entries.tail)

  override def toString: String =
    if (entries.isEmpty) {
      "<empty>"
    } else {
      entries.mkString("JsonPath(", " \\ ", ")")
    }
}

object JsonPath {
  implicit val rw: RW[JsonPath] = RW.from[JsonPath](
    r = path =>
      obj(
        "entries" -> path.entries.map { entry =>
          JsonPathEntry.rw.read(entry)
        }
      ),
    w = json => JsonPath(json("entries").asVector.toList.map(JsonPathEntry.rw.write)),
    d = DefType.Obj(
      Some("fabric.JsonPath"),
      "entries" -> DefType.Arr(JsonPathEntry.rw.definition)
    )
  )

  lazy val empty: JsonPath = new JsonPath(Nil)

  def apply(entries: JsonPathEntry*): JsonPath = new JsonPath(entries.toList)

  /**
    * Simple splitting functionality to separate a string into a path by
    * separation character.
    *
    * The separation character defaults to '.'
    */
  def parse(path: String, sep: Char = '.'): JsonPath = new JsonPath(
    path.split(sep).map(_.trim).filter(_ != "").map(JsonPathEntry.Named.apply).toList
  )
}
