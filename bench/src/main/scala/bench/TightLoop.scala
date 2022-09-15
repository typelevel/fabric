package bench

import fabric.io.{FormatParser, JacksonParser}

object TightLoop {
  private val jsonString: String =
    """{
      | "string": "Sample String",
      | "int": 123,
      | "seq": [1, 2, 3],
      | "object": {
      |   "one": 1,
      |   "two": "Dos",
      |   "three": "III"
      | }
      |}""".stripMargin

  private def parseFabric(jsonString: String, parser: FormatParser, count: Int): Unit = {
    (0 until count).foreach { _ =>
      val value = parser(jsonString)
      assert(value.isObj)
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = JacksonParser
    val count = 20_000_000

    val start = System.currentTimeMillis()
    parseFabric(jsonString, parser, count)
    val finish = System.currentTimeMillis()
    val elapsed = (finish - start) / 1000.0
    val perSecond = math.round(count / elapsed)
    println(s"Processed $count in $elapsed seconds ($perSecond per second)")
  }
}