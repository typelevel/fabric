package fabric.rw

import fabric.{Json, Obj}

import scala.collection.immutable.ListMap

trait ClassR[T] extends Reader[T] {
  protected def t2Map(t: T): ListMap[String, Json]

  override def read(t: T): Json = Obj(t2Map(t))
}