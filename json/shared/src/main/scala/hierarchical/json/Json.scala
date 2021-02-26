package hierarchical.json

import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonParser, JsonToken}
import hierarchical._

import java.io.ByteArrayOutputStream
import scala.annotation.tailrec

/**
 * Json provides support for parsing and formatting hierarchical Values from/to JSON
 */
object Json {
  private lazy val factory = new JsonFactory()
    .enable(JsonParser.Feature.ALLOW_COMMENTS)
    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)

  /**
   * Formats the supplied value for pretty output.
   *
   * @param value the value to format
   * @return formatted String
   */
  def format(value: Value): String = {
    val output = new ByteArrayOutputStream
    try {
      val gen = factory.createGenerator(output)
      try {
        gen.setPrettyPrinter(new DefaultPrettyPrinter {
          _objectFieldValueSeparatorWithSpaces = ": "
        })
        format(gen, value)
        gen.flush()
      } finally {
        gen.close()
      }
      output.flush()
      output.toString("UTF-8")
    } finally {
      output.close()
    }
  }

  /**
   * Parses the JSON string into a hierarchical Value.
   *
   * @param s the JSON string to parse
   * @return Value
   */
  def parse(s: String): Value = {
    val parser = factory.createParser(s)
    try {
      parse(parser)
    } finally {
      parser.close()
    }
  }

  protected def format(gen: JsonGenerator, value: Value): Unit = value match {
    case Obj(map) => {
      gen.writeStartObject()
      map.foreach {
        case (key, value) => {
          gen.writeFieldName(key)
          format(gen, value)
        }
      }
      gen.writeEndObject()
    }
    case Arr(vec) => {
      gen.writeStartArray()
      vec.foreach { value =>
        format(gen, value)
      }
      gen.writeEndArray()
    }
    case Bool(b) => gen.writeBoolean(b)
    case Num(n) => gen.writeNumber(n)
    case Str(s) => gen.writeString(s)
    case Null => gen.writeNull()
  }

  protected def parse(parser: JsonParser): Value = parseToken(parser, parser.nextToken())

  private def parseToken(parser: JsonParser, token: JsonToken): Value = token match {
    case JsonToken.START_OBJECT => parseObj(parser, Map.empty)
    case JsonToken.START_ARRAY => parseArr(parser, Nil)
    case JsonToken.VALUE_STRING => Str(parser.getValueAsString)
    case JsonToken.VALUE_NUMBER_FLOAT | JsonToken.VALUE_NUMBER_INT => Num(parser.getValueAsDouble)
    case JsonToken.VALUE_NULL => Null
    case JsonToken.VALUE_TRUE => Bool(true)
    case JsonToken.VALUE_FALSE => Bool(false)
    case t => throw new RuntimeException(s"Unsupported token: $t")
  }

  @tailrec
  private def parseObj(parser: JsonParser, map: Map[String, Value]): Obj = {
    val next = parser.nextToken()
    if (next == JsonToken.END_OBJECT) {
      Obj(map)
    } else {
      val key = parser.getCurrentName
      val value = parse(parser)
      parseObj(parser, map + (key -> value))
    }
  }

  @tailrec
  private def parseArr(parser: JsonParser, list: List[Value]): Arr = {
    val next = parser.nextToken()
    if (next == JsonToken.END_ARRAY) {
      Arr(list.reverse.toVector)
    } else {
      val value = parseToken(parser, next)
      parseArr(parser, value :: list)
    }
  }
}