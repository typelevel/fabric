package fabric.io

sealed trait Format

object Format {
  case object Json extends Format
  case object Yaml extends Format
  case object XML extends Format
  case object Hocon extends Format
  case object Properties extends Format
}