package bench

import fabric.io._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// jmh:run -i 3 -wi 3 -f1 -t1 -rf JSON -rff benchmarks.json
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class ParseJsonSmall extends AbstractParseJson {
  @Benchmark
  def fabricJacksonSmall(): Unit = parseFabric(Samples.smallJsonString, JacksonParser)

  @Benchmark
  def fabricJsoniterSmall(): Unit = parseFabric(Samples.smallJsonString, JsoniterParser)

  @Benchmark
  def fabricSimpleJsonSmall(): Unit = parseFabric(Samples.smallJsonString, SimpleJsonParser)

  @Benchmark
  def circeSmall(): Unit = parseCirce(Samples.smallJsonString)

  @Benchmark
  def uJsonSmall(): Unit = parseUJson(Samples.smallJsonString)
}