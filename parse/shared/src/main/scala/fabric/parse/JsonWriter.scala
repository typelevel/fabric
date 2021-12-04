package fabric.parse

import fabric._

trait JsonWriterConfig {
  def newLine(): String
  def indent(depth: Int): String
  def keyValueSeparator(): String = ": "

  def encodeString(s: String): String = {
    val e = s
      .replace("\n", "\\n")
      .replace("\"", "\\\"")
    s""""$e""""
  }
}

object JsonWriterConfig {
  case class Standard(indent: Int = 2) extends JsonWriterConfig {
    override def newLine(): String = "\n"
    override def indent(depth: Int): String = "".padTo(depth * this.indent, ' ')
  }

  case object Compact extends JsonWriterConfig {
    override def newLine(): String = ""
    override def indent(depth: Int): String = ""
    override def keyValueSeparator(): String = ":"
  }
}

case class JsonWriter(config: JsonWriterConfig) { w =>
  def apply(value: Value): String = {
    write(value, 0)
  }

  private def write(value: Value, depth: Int): String = value match {
    case Arr(v) =>
      val content = v.map { value =>
        s"${config.newLine()}${config.indent(depth + 1)}${write(value, depth + 1)}"
      }.mkString(",")
      s"[$content${config.newLine()}${config.indent(depth)}]"
    case Bool(b) => b.toString
    case Null => "null"
    case NumInt(n) => n.toString
    case NumDec(n) => n.toString()
    case Obj(map) =>
      val content = map.toList.map {
        case (key, value) =>
          s"${config.newLine()}${config.indent(depth + 1)}${config.encodeString(key)}${config.keyValueSeparator()}${write(value, depth + 1)}"
      }.mkString(",")
      s"{$content${config.newLine()}${config.indent(depth)}}"
    case Str(s) => config.encodeString(s)
  }
}

object JsonWriter {
  lazy val Default: JsonWriter = JsonWriter(JsonWriterConfig.Standard())
  lazy val Compact: JsonWriter = JsonWriter(JsonWriterConfig.Compact)
}