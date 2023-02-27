package bench

import fabric.Json
import fabric.io.{Format, JsonParser}

trait AbstractCaseClassConvert {
  val count: Int = 1000

  def json: String

  lazy val fabricJson: Json = JsonParser(json, Format.Json)
  lazy val circeJson: io.circe.Json = io.circe.parser.parse(json).getOrElse(throw new RuntimeException("Parse Error"))
  lazy val uPickleJson: ujson.Value = ujson.read(json)

  object uPickleSupport extends upickle.AttributeTagged {
    implicit override def OptionWriter[T: Writer]: Writer[Option[T]] = implicitly[Writer[T]].comap[Option[T]] {
      case None => null.asInstanceOf[T]
      case Some(x) => x
    }

    implicit override def OptionReader[T: Reader]: Reader[Option[T]] =
      new Reader.Delegate[Any, Option[T]](implicitly[Reader[T]].map(Some(_))) {
        override def visitNull(index: Int): Option[T] = None
      }
  }
}
