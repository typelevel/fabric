package bench

import fabric.io._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// jmh:run -i 3 -wi 3 -f1 -t1 -rf JSON -rff benchmarks.json
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class ParseJsonLarge extends AbstractParseJson {
  @Benchmark
  def fabricJacksonLarge(): Unit =
    parseFabric(Samples.largeJsonString, JacksonParser)

  @Benchmark
  def fabricJsoniterLarge(): Unit =
    parseFabric(Samples.largeJsonString, JsoniterParser)

  @Benchmark
  def fabricSimpleJsonLarge(): Unit =
    parseFabric(Samples.largeJsonString, SimpleJsonParser)

  @Benchmark
  def circeLarge(): Unit = parseCirce(Samples.largeJsonString)

  @Benchmark
  def uJsonLarge(): Unit = parseUJson(Samples.largeJsonString)
}
