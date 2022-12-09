package scala.collection.immutable

object VectorMap {
  def apply[Key, Value](entries: (Key, Value)*): Map[Key, Value] = ListMap(entries: _*)

  def empty[Key, Value]: Map[Key, Value] = apply[Key, Value]()
}