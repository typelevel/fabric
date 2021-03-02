package hierarchical.parse

import hierarchical._

import java.io.StringReader

import scala.jdk.CollectionConverters._

object Properties extends Parser {
  override def parse(content: String): Value = {
    val p = new java.util.Properties
    p.load(new StringReader(content))
    parse(p)
  }

  // TODO: Should I extract this out?
  def parse(properties: java.util.Properties): Value = {
    var v: Value = obj()

    properties.asScala.foreach {
      case (key, value) => {
        val path = new Path(key.split('.').toList)
        v = v.merge(path, str(value))
      }
    }
    v
  }
}