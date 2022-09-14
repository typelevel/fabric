package fabric.io

import fabric.Json

import scala.io.Source

trait Parser {
  final def apply(source: Source, format: Format): Json = {
    try {
      apply(source.mkString("\n"), format)
    } finally {
      source.close()
    }
  }

  def apply(content: String, format: Format): Json
}