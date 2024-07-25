package spec

import fabric.*
import fabric.rw.Convertible
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeltaSpec extends AnyWordSpec with Matchers {
  "Delta" should {
    "determine numeric changes" in {
      val json1: Json = 5
      val json2: Json = 6
      Delta.changed(json1, json2) should be(Some(6.json))
      Delta.diff(json1, json2) should be(Some(obj(
        "old" -> 5,
        "new" -> 6
      )))
    }
    "determine simple obj changes" in {
      val json1 = obj(
        "s" -> "Hello",
        "i" -> 5,
        "c" -> "Test",
        "r" -> "Value"
      )
      val json2 = obj(
        "s" -> "Goodbye",
        "i" -> 6,
        "c" -> "Test"
      )
      Delta.changed(json1, json2) should be(Some(obj(
        "s" -> "Goodbye",
        "i" -> 6,
        "r" -> Null
      )))
      Delta.diff(json1, json2) should be(Some(obj(
        "s" -> obj(
          "old" -> "Hello",
          "new" -> "Goodbye"
        ),
        "i" -> obj(
          "old" -> 5,
          "new" -> 6
        ),
        "r" -> obj(
          "old" -> "Value",
          "new" -> Null
        )
      )))
    }
    "determine simple arr changes" in {
      val json1 = arr(1, 2, 3, 4)
      val json2 = arr(2, 3, 4)
      Delta.changed(json1, json2) should be(Some(arr(
        2, 3, 4, Null
      )))
      Delta.diff(json1, json2) should be(Some(arr(
        obj("old" -> 1, "new" -> 2),
        obj("old" -> 2, "new" -> 3),
        obj("old" -> 3, "new" -> 4),
        obj("old" -> 4, "new" -> Null)
      )))
    }
  }
}
