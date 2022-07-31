package fabric.parse

import fabric._

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.xml.XmlFactory

object XML extends Parser {
  override def parse(value: String): Json = {
    val reader = new ObjectMapper(new XmlFactory)
    val obj = reader.readValue(s"<xml>$value</xml>", classOf[java.lang.Object])
    val writer = new ObjectMapper()
    val jsonString = writer.writeValueAsString(obj)
    JsonParser.parse(jsonString)
  }
}