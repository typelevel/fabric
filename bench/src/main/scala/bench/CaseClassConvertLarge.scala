package bench

import fabric.rw._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@OperationsPerInvocation(1000)
class CaseClassConvertLarge extends AbstractCaseClassConvert {
  val json: String = Samples.largeJsonString

  @Benchmark
  def fabric(): Unit = (0 until count).foreach { _ =>
    val events = fabricJson.as[List[Event]]
    assert(events.size == 30)
  }

  @Benchmark
  def circe(): Unit = (0 until count).foreach { _ =>
    val events = circeJson.as[List[Event]].getOrElse(throw new RuntimeException("Unable to convert to User"))
    assert(events.size == 30)
  }

  @Benchmark
  def uPickle(): Unit = (0 until count).foreach { _ =>
    val events = uPickleSupport.read[List[Event]](uPickleJson)
    assert(events.size == 30)
  }

  case class Event(id: String,
                   `type`: String,
                   actor: EventActor,
                   repo: EventRepo,
                   public: Boolean,
                   created_at: String)
  object Event {
    implicit val hRW: RW[Event] = ccRW
    implicit val cDecoder: io.circe.Decoder[Event] = io.circe.generic.semiauto.deriveDecoder[Event]
    implicit val uRW: uPickleSupport.ReadWriter[Event] = uPickleSupport.macroRW[Event]
  }
  case class EventActor(id: Long,
                        login: String,
                        display_login: String,
                        gravatar_id: String,
                        url: String,
                        avatar_url: String)
  object EventActor {
    implicit val hRW: RW[EventActor] = ccRW
    implicit val cDecoder: io.circe.Decoder[EventActor] = io.circe.generic.semiauto.deriveDecoder[EventActor]
    implicit val uRW: uPickleSupport.ReadWriter[EventActor] = uPickleSupport.macroRW[EventActor]
  }
  case class EventRepo(id: Long,
                       name: String,
                       url: String)
  object EventRepo {
    implicit val hRW: RW[EventRepo] = ccRW
    implicit val cDecoder: io.circe.Decoder[EventRepo] = io.circe.generic.semiauto.deriveDecoder[EventRepo]
    implicit val uRW: uPickleSupport.ReadWriter[EventRepo] = uPickleSupport.macroRW[EventRepo]
  }
}