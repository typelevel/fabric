package bench

import cats.effect.unsafe.implicits.global
import fabric.io.{Format, JsonParser}
import fabric.rw._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class CaseClassConvert {
  private val count: Int = 1000

  private val smallJson =
    """{
      |   "name": "A Person",
      |   "age": 123
      |}""".stripMargin
  private val mediumJson =
    """{
      |   "username": "user123",
      |   "password": "password",
      |   "fullName": "A Person",
      |   "addresses": [
      |     {"city": "San Jose", "state": "California"},
      |     {"city": "Norman", "state": "Oklahoma"}
      |   ]
      |}""".stripMargin
  private lazy val hSmallJson: fabric.Json = JsonParser(smallJson, Format.Json).unsafeRunSync()
  private lazy val hMediumJson: fabric.Json = JsonParser(mediumJson, Format.Json).unsafeRunSync()
  private lazy val cSmallJson: io.circe.Json = io.circe.parser.parse(smallJson).getOrElse(throw new RuntimeException("Parse Error"))
  private lazy val cMediumJson: io.circe.Json = io.circe.parser.parse(mediumJson).getOrElse(throw new RuntimeException("Parse Error"))
  private lazy val uSmallJson: ujson.Value = ujson.read(smallJson)
  private lazy val uMediumJson: ujson.Value = ujson.read(mediumJson)

  @Benchmark
  def fabricSmall(): Unit = (0 until count).foreach { _ =>
    val person = hSmallJson.as[Person]
    assert(person.age == 123)
  }

  @Benchmark
  def fabricMedium(): Unit = (0 until count).foreach { _ =>
    val user = hMediumJson.as[User]
    assert(user.fullName.contains("A Person"))
  }

  @Benchmark
  def circeSmall(): Unit = (0 until count).foreach { _ =>
    val person = cSmallJson.as[Person].getOrElse(throw new RuntimeException("Unable to convert to Person"))
    assert(person.age == 123)
  }

  @Benchmark
  def circeMedium(): Unit = (0 until count).foreach { _ =>
    val user = cMediumJson.as[User].getOrElse(throw new RuntimeException("Unable to convert to User"))
    assert(user.fullName.contains("A Person"))
  }

  @Benchmark
  def uPickleSmall(): Unit = (0 until count).foreach { _ =>
    val person = uPickle.read[Person](uSmallJson)
    assert(person.age == 123)
  }

  @Benchmark
  def uPickleMedium(): Unit = (0 until count).foreach { _ =>
    val user = uPickle.read[User](uMediumJson)
    assert(user.fullName.contains("A Person"))
  }

  case class Person(name: String, age: Int)
  object Person {
    implicit val hRW: ReaderWriter[Person] = ccRW[Person]
    implicit val cDecoder: io.circe.Decoder[Person] = io.circe.generic.semiauto.deriveDecoder[Person]
    implicit val uRW: uPickle.ReadWriter[Person] = uPickle.macroRW[Person]
  }
  case class Address(city: String, state: String)
  object Address {
    implicit val hRW: ReaderWriter[Address] = ccRW[Address]
    implicit val cDecoder: io.circe.Decoder[Address] = io.circe.generic.semiauto.deriveDecoder[Address]
    implicit val uRW: uPickle.ReadWriter[Address] = uPickle.macroRW[Address]
  }
  case class User(username: String, password: String, fullName: Option[String], addresses: List[Address])
  object User {
    implicit val hRW: ReaderWriter[User] = ccRW[User]
    implicit val cDecoder: io.circe.Decoder[User] = io.circe.generic.semiauto.deriveDecoder[User]
    implicit val uRW: uPickle.ReadWriter[User] = uPickle.macroRW[User]
  }
  object uPickle extends upickle.AttributeTagged {
    override implicit def OptionWriter[T: Writer]: Writer[Option[T]] =
      implicitly[Writer[T]].comap[Option[T]] {
        case None => null.asInstanceOf[T]
        case Some(x) => x
      }

    override implicit def OptionReader[T: Reader]: Reader[Option[T]] = {
      new Reader.Delegate[Any, Option[T]](implicitly[Reader[T]].map(Some(_))){
        override def visitNull(index: Int): Option[T] = None
      }
    }
  }
}
