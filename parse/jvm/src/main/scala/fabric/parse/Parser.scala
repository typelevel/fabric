package fabric.parse

import fabric.Json

import scala.io.Source

trait Parser {
  def parse(source: Source): Json = try {
    val s = source.mkString("\n")
    parse(s)
  } finally {
    source.close()
  }

  def parse(content: String): Json
}

object Parser {
  def apply(source: Source, format: Format): Json = format match {
    case Format.Hocon => Hocon.parse(source)
    case Format.Json => JsonParser.parse(source)
    case Format.Properties => Properties.parse(source)
    case Format.XML => XML.parse(source)
    case Format.Yaml => Yaml.parse(source)
  }

  def apply(s: String, format: Format): Json = format match {
    case Format.Hocon => Hocon.parse(s)
    case Format.Json => JsonParser.parse(s)
    case Format.Properties => Properties.parse(s)
    case Format.XML => XML.parse(s)
    case Format.Yaml => Yaml.parse(s)
  }
}