package fabric.io

import cats.effect.IO
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import fabric._

object YamlParser extends FormatParser {
  override def format: Format = Format.Yaml

  override def apply(content: String): IO[Json] = IO {
    val reader = new ObjectMapper(new YAMLFactory)
    val obj = reader.readValue(content, classOf[java.lang.Object])
    val writer = new ObjectMapper()
    writer.writeValueAsString(obj)
  }.flatMap { jsonString =>
    JsonParser(jsonString, Format.Json)
  }
}