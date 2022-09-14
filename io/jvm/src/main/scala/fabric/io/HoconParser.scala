package fabric.io

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import fabric.Json

object HoconParser extends FormatParser {
  override def format: Format = Format.Hocon

  override def apply(content: String): Json = {
    val conf = ConfigFactory.parseString(content).resolve()
    val jsonString = conf.root().render(ConfigRenderOptions.concise())
    JsonParser(jsonString, Format.Json)
  }
}
