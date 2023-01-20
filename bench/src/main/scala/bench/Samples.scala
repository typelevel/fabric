package bench

import scala.io.Source

object Samples {
  val smallJsonString: String = """{"name": "Sample", "age": 123}"""
  val mediumJsonString: String =
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
  val largeJsonString: String = {
    val source = Source.fromInputStream(
      getClass.getClassLoader.getResourceAsStream("large.json")
    )
    try {
      source.mkString
    } finally {
      source.close()
    }
  }
}
