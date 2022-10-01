package fabric.rw

import fabric.Json

import scala.collection.immutable.ListMap

trait ClassW[T] extends Writer[T] {
  protected def map2T(map: ListMap[String, Json]): T

  override def write(value: Json): T = map2T(value.asMap)
}