package fabric.rw

import fabric.{Obj, Value}

trait ClassR[T] extends Reader[T] {
  protected def t2Map(t: T): Map[String, Value]

  override def read(t: T): Value = Obj(t2Map(t))
}