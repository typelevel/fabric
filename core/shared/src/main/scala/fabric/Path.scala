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

/**
 * Path is a convenience wrapper to represent paths for lookups or changes in Json
 */
case class Path(entries: List[String]) extends AnyVal {
  /**
   * Convenience DSL to build paths
   */
  def \(entry: String): Path = new Path(entries ::: List(entry))

  def \\(that: Path): Path = new Path(entries ::: that.entries)

  def isEmpty: Boolean = entries.isEmpty

  def nonEmpty: Boolean = entries.nonEmpty

  /**
   * Retrieves the head path element
   */
  def apply(): String = entries.head

  /**
   * Returns a new Path with the tail of this path
   */
  def next(): Path = new Path(entries.tail)

  override def toString: String = if (entries.isEmpty) {
    "<empty>"
  } else {
    entries.mkString("Path(", " \\ ", ")")
  }
}

object Path {
  lazy val empty: Path = new Path(Nil)

  def apply(entries: String*): Path = new Path(entries.toList)

  /**
   * Simple splitting functionality to separate a string into a path by separation character.
   *
   * The separation character defaults to '.'
   */
  def parse(path: String, sep: Char = '.'): Path = new Path(path.split(sep).map(_.trim).filter(_ != "").toList)
}