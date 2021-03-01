package hierarchical.parse

import hierarchical._

import scala.io.Source

/**
 * Json provides support for parsing and formatting hierarchical Values from/to JSON
 */
trait AbstractJson {
  /**
   * Formats the supplied value for pretty output.
   *
   * @param value the value to format
   * @return formatted String
   */
  def format(value: Value): String

  /**
   * Parses the JSON string into a hierarchical Value.
   *
   * @param s the JSON string to parse
   * @return Value
   */
  def parse(s: String): Value

  /**
   * Parses the JSON source into a hierarchical Value.
   *
   * @param source the source of JSON to parse
   * @return Value
   */
  def parse(source: Source): Value = try {
    parse(source.mkString("\n"))
  } finally {
    source.close()
  }
}