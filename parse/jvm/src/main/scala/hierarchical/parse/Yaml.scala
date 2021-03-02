package hierarchical.parse

import hierarchical._

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory

object Yaml extends Parser {
  override def parse(value: String): Value = {
    val reader = new ObjectMapper(new YAMLFactory)
    val obj = reader.readValue(value, classOf[java.lang.Object])
    val writer = new ObjectMapper()
    val jsonString = writer.writeValueAsString(obj)
    Json.parse(jsonString)
  }
}