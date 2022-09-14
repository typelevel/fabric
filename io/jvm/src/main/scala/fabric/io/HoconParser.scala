package fabric.io

import cats.effect.IO
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import fabric.Json

object HoconParser extends FormatParser {
  override def format: Format = Format.Hocon

  override def apply(content: String): IO[Json] = IO {
    val conf = ConfigFactory.parseString(content).resolve()
    conf.root().render(ConfigRenderOptions.concise())
  }.flatMap { jsonString =>
    JsonParser(jsonString, Format.Json)
  }
}
