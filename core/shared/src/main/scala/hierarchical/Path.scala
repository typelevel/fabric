package hierarchical

/**
 * Path is a convenience wrapper to represent paths for lookups or changes in Value
 */
class Path(val entries: List[String]) extends AnyVal {
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

  override def toString: String = entries.mkString(".")
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