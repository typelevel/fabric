package fabric.io

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.xml.XmlFactory
import fabric._

object XMLParser extends FormatParser {
  override def format: Format = Format.XML

  override def apply(content: String): Json = {
    val reader = new ObjectMapper(new XmlFactory)
    val obj = reader.readValue(s"<xml>$content</xml>", classOf[java.lang.Object])
    val writer = new ObjectMapper()
    val jsonString = writer.writeValueAsString(obj)
    JsonParser(jsonString, Format.Json)
  }
}