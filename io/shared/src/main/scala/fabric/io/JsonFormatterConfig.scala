package fabric.io

trait JsonFormatterConfig {
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

object JsonFormatterConfig {
  case class Standard(indent: Int = 2) extends JsonFormatterConfig {
    override def newLine(): String = "\n"

    override def indent(depth: Int): String = "".padTo(depth * this.indent, ' ')
  }

  case object Compact extends JsonFormatterConfig {
    override def newLine(): String = ""

    override def indent(depth: Int): String = ""

    override def keyValueSeparator(): String = ":"
  }
}