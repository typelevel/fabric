package hierarchical

class Path(val entries: List[String]) extends AnyVal {
  def \(entry: String): Path = new Path(entries ::: List(entry))

  def isEmpty: Boolean = entries.isEmpty

  def nonEmpty: Boolean = entries.nonEmpty

  def apply(): String = entries.head

  def next(): Path = new Path(entries.tail)

  override def toString: String = entries.mkString(".")
}