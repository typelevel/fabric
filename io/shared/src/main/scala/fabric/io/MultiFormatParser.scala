package fabric.io

import fabric.Json

trait MultiFormatParser extends Parser {
  def parsers: List[FormatParser]

  private lazy val map: Map[Format, FormatParser] = parsers.map { parser =>
    parser.format -> parser
  }.toMap

  override def apply(content: String, format: Format): Json = map.get(format) match {
    case Some(parser) => parser(content)
    case None => throw new RuntimeException(s"Format not supported: $format")
  }
}
