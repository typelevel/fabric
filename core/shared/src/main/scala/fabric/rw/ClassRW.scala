package fabric.rw

import fabric.{Obj, Value}

/**
 * ClassRW provides convenience functionality to simplify class mapping with ReaderWriter
 */
trait ClassRW[T] extends ReaderWriter[T] {
  protected def t2Map(t: T): Map[String, Value]
  protected def map2T(map: Map[String, Value]): T

  override def read(t: T): Value = Obj(t2Map(t))
  override def write(value: Value): T = map2T(value.asObj.value)
}
