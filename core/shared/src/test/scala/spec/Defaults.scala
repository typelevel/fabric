package spec

import fabric.rw._

case class Defaults(name: String = "John Doe", age: Int = 21)

object Defaults {
  implicit val rw: RW[Defaults] = ccRW
}