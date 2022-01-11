package spec

import fabric.rw._

case class Person(name: String, age: Int, address: Address)

object Person {
  implicit val rw: RW[Person] = ccRW
}