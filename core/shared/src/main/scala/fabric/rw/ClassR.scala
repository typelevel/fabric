package fabric.rw

import fabric.{Obj, Json}

trait ClassR[T] extends Reader[T] {
  protected def t2Map(t: T): Map[String, Json]

  override def read(t: T): Json = Obj(t2Map(t))
}