package fabric.io

import fabric.{Arr, Bool, Json, Null, NumDec, NumInt, Obj, Str}

case class JsonFormatter(config: JsonFormatterConfig) extends Formatter {
  override def format: Format = Format.Json

  def apply(value: Json): String = write(value, 0)

  private def write(value: Json, depth: Int): String = value match {
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

object JsonFormatter {
  lazy val Default: JsonFormatter = JsonFormatter(JsonFormatterConfig.Standard())
  lazy val Compact: JsonFormatter = JsonFormatter(JsonFormatterConfig.Compact)
}