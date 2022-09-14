package bench

import cats.effect.unsafe.implicits.global
import fabric.io._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// jmh:run -i 3 -wi 3 -f1 -t1 -rf JSON -rff benchmarks.json
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class ParseJson {
  private val count: Int = 1000
  private val smallJsonString: String = """{"name": "Sample", "age": 123}"""
  private val mediumJsonString: String =
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

  @Benchmark
  def fabricJacksonSmall(): Unit = parseFabric(smallJsonString, JacksonParser)

  @Benchmark
  def fabricJsoniterSmall(): Unit = parseFabric(smallJsonString, JsoniterParser)

  @Benchmark
  def fabricJacksonMedium(): Unit = parseFabric(mediumJsonString, JacksonParser)

  @Benchmark
  def fabricJsoniterMedium(): Unit = parseFabric(mediumJsonString, JsoniterParser)

  @Benchmark
  def circeSmall(): Unit = parseCirce(smallJsonString)

  @Benchmark
  def circeMedium(): Unit = parseCirce(mediumJsonString)

  @Benchmark
  def uJsonSmall(): Unit = parseUJson(smallJsonString)

  @Benchmark
  def uJsonMedium(): Unit = parseUJson(mediumJsonString)

  private def parseFabric(jsonString: String, parser: FormatParser): Unit = {
    (0 until count).foreach { _ =>
      val value = parser(jsonString).unsafeRunSync()
      assert(value.isObj)
    }
  }

  def parseCirce(jsonString: String): Unit = {
    import io.circe.parser._

    (0 until count).foreach { _ =>
      val value = parse(jsonString)
      assert(value.isRight)
    }
  }

  def parseUJson(jsonString: String): Unit = {
    (0 until count).foreach { _ =>
      val value = ujson.read(jsonString)
      assert(value.objOpt.nonEmpty)
    }
  }
}