package bench

import fabric.rw._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class CaseClassConvertMedium extends AbstractCaseClassConvert {
  val json: String =
    """{
      |   "username": "user123",
      |   "password": "password",
      |   "fullName": "A Person",
      |   "addresses": [
      |     {"city": "San Jose", "state": "California"},
      |     {"city": "Norman", "state": "Oklahoma"}
      |   ]
      |}""".stripMargin

  @Benchmark
  def fabric(): Unit = (0 until count).foreach { _ =>
    val user = fabricJson.as[User]
    assert(user.fullName.contains("A Person"))
  }

  @Benchmark
  def circe(): Unit = (0 until count).foreach { _ =>
    val user = circeJson.as[User].getOrElse(throw new RuntimeException("Unable to convert to User"))
    assert(user.fullName.contains("A Person"))
  }

  @Benchmark
  def uPickle(): Unit = (0 until count).foreach { _ =>
    val user = uPickleSupport.read[User](uPickleJson)
    assert(user.fullName.contains("A Person"))
  }

  case class Address(city: String, state: String)
  object Address {
    implicit val hRW: RW[Address] = RW.gen
    implicit val cDecoder: io.circe.Decoder[Address] = io.circe.generic.semiauto.deriveDecoder[Address]
    implicit val uRW: uPickleSupport.ReadWriter[Address] = uPickleSupport.macroRW[Address]
  }
  case class User(username: String, password: String, fullName: Option[String], addresses: List[Address])
  object User {
    implicit val hRW: RW[User] = RW.gen
    implicit val cDecoder: io.circe.Decoder[User] = io.circe.generic.semiauto.deriveDecoder[User]
    implicit val uRW: uPickleSupport.ReadWriter[User] = uPickleSupport.macroRW[User]
  }
}
