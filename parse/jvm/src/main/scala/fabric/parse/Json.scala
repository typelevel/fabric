package fabric.parse

import fabric.Value

import java.io.File
import java.nio.file.Path
import scala.io.Source

/**
 * Json provides convenience functionality to parse and format JSON to/from fabric Values
 */
object Json extends AbstractJson {
  override def parse(s: String): Value = JsoniterParser.parse(s)

  def parse(file: File): Value = parse(Source.fromFile(file, "UTF-8"))
  def parse(path: Path): Value = parse(Source.fromFile(path.toFile, "UTF-8"))
}