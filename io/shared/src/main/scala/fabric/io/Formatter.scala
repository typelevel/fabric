package fabric.io

import cats.effect.IO
import fabric._

trait Formatter {
  def format: Format

  def apply(json: Json): IO[String]
}







