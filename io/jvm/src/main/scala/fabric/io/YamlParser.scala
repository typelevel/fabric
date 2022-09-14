package fabric.io

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import fabric._

object YamlParser extends FormatParser {
  override def format: Format = Format.Yaml

  override def apply(content: String): Json = {
    val reader = new ObjectMapper(new YAMLFactory)
    val obj = reader.readValue(content, classOf[java.lang.Object])
    val writer = new ObjectMapper()
    val jsonString = writer.writeValueAsString(obj)
    JsonParser(jsonString, Format.Json)
  }
}