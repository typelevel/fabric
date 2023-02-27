package bench

import fabric.io.FormatParser

trait AbstractParseJson {
  val count: Int = 1000

  def parseFabric(jsonString: String, parser: FormatParser): Unit = (0 until count).foreach {
    _ =>
      val value = parser(jsonString)
      assert(value.isObj || value.isArr)
  }

  def parseCirce(jsonString: String): Unit = {
    import io.circe.parser._

    (0 until count).foreach {
      _ =>
        val value = parse(jsonString)
        assert(value.isRight)
    }
  }

  def parseUJson(jsonString: String): Unit = (0 until count).foreach {
    _ =>
      val value = ujson.read(jsonString)
      assert(value.objOpt.nonEmpty || value.arrOpt.nonEmpty)
  }
}
