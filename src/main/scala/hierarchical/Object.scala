package hierarchical

import scala.language.implicitConversions

object Object {
  def main(args: Array[String]): Unit = {
    val v: Obj = obj(
      "name" -> "Matt \"Matteo\" Hicks",
      "age" -> 41,
      "numbers" -> List(1, 2, 3),
      "address" -> obj(
        "street" -> "123 Somewhere Rd.\nBox 123",
        "city" -> "San Jose",
        "state" -> "California",
        "zipcode" -> 95136
      )
    )
    println(v)
    val state = v("address" \ "state")
    println(s"State: $state")
    val updated = v.modify("address" \ "state") { value =>
      println(s"Updating: $value")
      str("Tennessee")
    }
    println(s"Updated: $updated")
    println(s"Original: $v")
    val removed = v.remove("address" \ "state")
    println(s"Removed: $removed")
  }
}