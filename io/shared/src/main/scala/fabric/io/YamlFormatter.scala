package fabric.io

import fabric._

object YamlFormatter extends Formatter {
  override def format: Format = Format.Yaml

  override def apply(json: Json): String = write(json, 0).trim

  private def write(json: Json, depth: Int): String = {
    def pad(adjust: Int = 0): String = "".padTo((depth + adjust) * 2, ' ')
    json match {
      case Arr(v) => v.map(write(_, depth)).map(s => s"${pad(-1)}- $s").mkString("\n", "\n", "")
      case Bool(b) => b.toString
      case Null => ""
      case NumInt(n) => n.toString
      case NumDec(n) => n.toString()
      case Obj(map) =>
        map.toList.map {
          case (key, value) =>
            s"${pad()}$key: ${write(value, depth + 1)}"
        }.mkString("\n", "\n", "")
      case Str(s) if s.contains("\n") => s.split('\n').map(s => s"${pad()}$s").mkString("|-\n", "\n", "")
      case Str(s) => s
    }
  }
}
