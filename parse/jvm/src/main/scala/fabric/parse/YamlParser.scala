package fabric.parse

import fabric._

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory

object YamlParser extends Parser {
  override def parse(value: String): Json = {
    val reader = new ObjectMapper(new YAMLFactory)
    val obj = reader.readValue(value, classOf[java.lang.Object])
    val writer = new ObjectMapper()
    val jsonString = writer.writeValueAsString(obj)
    JsonParser.parse(jsonString)
  }
}