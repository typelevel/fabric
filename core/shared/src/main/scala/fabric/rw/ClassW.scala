package fabric.rw

import fabric.Json

trait ClassW[T] extends Writer[T] {
  protected def map2T(map: Map[String, Json]): T

  override def write(value: Json): T = map2T(value.asObj.value)
}