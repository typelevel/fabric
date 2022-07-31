package fabric.parse

import fabric._

import scala.io.Source

/**
 * Json provides support for parsing and formatting fabric Values from/to JSON
 */
trait AbstractJsonParser {
  /**
   * Formats the supplied value for pretty output.
   *
   * @param value the value to format
   * @param writer the JsonWriter to use (defaults to JsonWriter.Default)
   * @return formatted String
   */
  def format(value: Json,
             writer: JsonWriter = JsonWriter.Default): String = writer(value)

  /**
   * Parses the JSON string into a fabric Value.
   *
   * @param s the JSON string to parse
   * @return Value
   */
  def parse(s: String): Json

  /**
   * Parses the JSON source into a fabric Value.
   *
   * @param source the source of JSON to parse
   * @return Value
   */
  def parse(source: Source): Json = try {
    parse(source.mkString("\n"))
  } finally {
    source.close()
  }
}