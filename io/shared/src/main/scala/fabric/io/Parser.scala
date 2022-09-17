package fabric.io

import fabric.Json

import scala.io.Source

trait Parser {
  final def apply(source: Source, format: Format): Json = {
    try {
      apply(source.mkString, format)
    } finally {
      source.close()
    }
  }

  final def apply(content: Array[Byte], format: Format): Json = {
    val s = new String(content, "UTF-8")
    apply(s, format)
  }

  def apply(content: String, format: Format): Json
}