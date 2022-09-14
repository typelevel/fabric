package fabric.io

import cats.effect.IO
import fabric.Json

import scala.io.Source

trait MultiFormatParser extends Parser {
  def parsers: List[FormatParser]

  private lazy val map: Map[Format, FormatParser] = parsers.map { parser =>
    parser.format -> parser
  }.toMap

  override def apply(content: String, format: Format): IO[Json] = IO(map.get(format)).flatMap {
    case Some(parser) => parser(content)
    case None => throw new RuntimeException(s"Format not supported: $format")
  }
}
