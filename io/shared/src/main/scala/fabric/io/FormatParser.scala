package fabric.io

import fabric.Json

trait FormatParser {
  def format: Format

  def apply(content: String): Json
}
