package hierarchical.rw

import hierarchical.{Obj, Value}

/**
 * ClassRW provides convenience functionality to simplify class mapping with ReadableWritable
 */
trait ClassRW[T] extends ReadableWritable[T] {
  protected def t2Map(t: T): Map[String, Value]
  protected def map2T(map: Map[String, Value]): T

  override def read(t: T): Value = Obj(t2Map(t))
  override def write(value: Value): T = map2T(value.asObj.value)
}
