package fabric.io

import cats.effect.IO
import fabric.Json

trait FormatParser {
  def format: Format

  def apply(content: String): IO[Json]
}
