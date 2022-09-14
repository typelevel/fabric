package fabric.io

import cats.effect.IO
import fabric.Json

import java.io.File
import java.nio.file.Path
import scala.io.Source

/**
 * Json provides convenience functionality to parse and format JSON to/from fabric Values
 */
object JsonParser extends MultiFormatParser {
  override def parsers: List[FormatParser] = List(
    HoconParser, JsoniterParser, PropertiesParser, XMLParser, YamlParser
  )

  def apply(file: File, format: Format): IO[Json] = apply(Source.fromFile(file, "UTF-8"), format)
  def apply(path: Path, format: Format): IO[Json] = apply(Source.fromFile(path.toFile, "UTF-8"), format)
}