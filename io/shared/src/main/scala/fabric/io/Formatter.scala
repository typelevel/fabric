package fabric.io

import fabric._

trait Formatter {
  def format: Format

  def apply(json: Json): String
}







