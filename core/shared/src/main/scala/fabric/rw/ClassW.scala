package fabric.rw

import fabric.Value

trait ClassW[T] extends Writer[T] {
  protected def map2T(map: Map[String, Value]): T

  override def write(value: Value): T = map2T(value.asObj.value)
}