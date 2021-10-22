package fabric.parse

import fabric._

case class JsonWriter(indent: Int = 2,
                      excludeNullValues: Boolean = false,
                      includeNewLines: Boolean = true,
                      compact: Boolean = false) { w =>
  private implicit class SBExtras(val b: StringBuilder) {
    def newLine(): Unit = if (includeNewLines && !compact) b.append('\n')
    def indent(depth: Int): Unit = if (includeNewLines && !compact) {
      b.append("".padTo(depth * w.indent, ' '))
    } else if (!compact) {
      b.append(' ')
    }

    def encoded(s: String): Unit = {
      b.append('"')
      val e = s
        .replace("\n", "\\n")
        .replace("\"", "\\\"")
      b.append(e)
      b.append('"')
    }
  }

  def apply(value: Value): String = {
    val b = new StringBuilder
    write(value, 0, b)
    b.toString()
  }

  private def write(value: Value, depth: Int, sb: StringBuilder): Unit = value match {
    case Arr(v) =>
      sb.append('[')
      var first = true
      v.foreach { value =>
        if (!first) sb.append(',')
        first = false
        sb.newLine()
        sb.indent(depth + 1)
        write(value, depth + 1, sb)
      }
      sb.newLine()
      sb.indent(depth)
      sb.append(']')
    case Bool(b) => sb.append(b)
    case Null => sb.append("null")
    case Num(n) => sb.append(n)
    case Obj(map) =>
      sb.append('{')
      var first = true
      map.foreach {
        case (key, value) =>
          if (value.isNull && excludeNullValues) {
            // Excluding null values
          } else {
            if (!first) sb.append(',')
            first = false
            sb.newLine()
            sb.indent(depth + 1)
            sb.encoded(key)
            sb.append(':')
            if (!compact) sb.append(' ')
            write(value, depth + 1, sb)
          }
      }
      sb.newLine()
      sb.indent(depth)
      sb.append('}')
    case Str(s) => sb.encoded(s)
  }
}

object JsonWriter {
  lazy val Default: JsonWriter = JsonWriter()
}