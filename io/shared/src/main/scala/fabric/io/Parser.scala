package fabric.io

import cats.effect.IO
import fabric.Json

import scala.io.Source

trait Parser {
  final def apply(source: Source, format: Format): IO[Json] = IO {
    try {
      source.mkString("\n")
    } finally {
      source.close()
    }
  }.flatMap { s =>
    apply(s, format)
  }

  def apply(content: String, format: Format): IO[Json]
}