package fabric.parse

import fabric._

import java.io.StringReader

import scala.jdk.CollectionConverters._

object Properties extends Parser {
  override def parse(content: String): Value = {
    val p = new java.util.Properties
    p.load(new StringReader(content))
    parse(p)
  }

  def parse(properties: java.util.Properties): Value = Obj.process(properties.asScala.toMap)
}