package fabric.parse

import fabric.Json

import java.io.File
import java.nio.file.Path
import scala.io.Source

/**
 * Json provides convenience functionality to parse and format JSON to/from fabric Values
 */
object JsonParser extends AbstractJsonParser {
  override def parse(s: String): Json = JsoniterParser.parse(s)

  def parse(file: File): Json = parse(Source.fromFile(file, "UTF-8"))
  def parse(path: Path): Json = parse(Source.fromFile(path.toFile, "UTF-8"))
}