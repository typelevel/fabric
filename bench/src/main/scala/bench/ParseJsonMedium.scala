package bench

import fabric.io._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// jmh:run -i 3 -wi 3 -f1 -t1 -rf JSON -rff benchmarks.json
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class ParseJsonMedium extends AbstractParseJson {
  @Benchmark
  def fabricJacksonMedium(): Unit = parseFabric(Samples.mediumJsonString, JacksonParser)

  @Benchmark
  def fabricJsoniterMedium(): Unit = parseFabric(Samples.mediumJsonString, JsoniterParser)

  @Benchmark
  def fabricSimpleJsonMedium(): Unit = parseFabric(Samples.mediumJsonString, SimpleJsonParser)

  @Benchmark
  def circeMedium(): Unit = parseCirce(Samples.mediumJsonString)

  @Benchmark
  def uJsonMedium(): Unit = parseUJson(Samples.mediumJsonString)
}
