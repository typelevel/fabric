package bench

import fabric.rw._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class CaseClassConvertSmall extends AbstractCaseClassConvert {
  val json: String =
    """{
      |   "name": "A Person",
      |   "age": 123
      |}""".stripMargin

  @Benchmark
  def fabric(): Unit = (0 until count).foreach { _ =>
    val person = fabricJson.as[Person]
    assert(person.age == 123)
  }

  @Benchmark
  def circe(): Unit = (0 until count).foreach { _ =>
    val person = circeJson.as[Person].getOrElse(throw new RuntimeException("Unable to convert to Person"))
    assert(person.age == 123)
  }

  @Benchmark
  def uPickle(): Unit = (0 until count).foreach { _ =>
    val person = uPickleSupport.read[Person](uPickleJson)
    assert(person.age == 123)
  }

  case class Person(name: String, age: Int)
  object Person {
    implicit val hRW: RW[Person] = ccRW[Person]
    implicit val cDecoder: io.circe.Decoder[Person] = io.circe.generic.semiauto.deriveDecoder[Person]
    implicit val uRW: uPickleSupport.ReadWriter[Person] = uPickleSupport.macroRW[Person]
  }
}
