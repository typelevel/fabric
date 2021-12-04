package spec

import fabric.rw._

case class Address(city: String, state: String)

object Address {
  implicit val rw: ReaderWriter[Address] = ccRW
}