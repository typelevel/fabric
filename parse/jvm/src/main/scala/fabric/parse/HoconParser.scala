package fabric.parse
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import fabric.Json

object HoconParser extends Parser {
  override def parse(content: String): Json = {
    val conf = ConfigFactory.parseString(content).resolve()
    val jsonString = conf.root().render(ConfigRenderOptions.concise())
    JsonParser.parse(jsonString)
  }
}
