package fabric.parse

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}
import fabric.{Arr, Bool, Null, Num, Obj, Str, Value}

import java.io.File
import java.nio.file.Path
import scala.annotation.tailrec
import scala.io.Source

/**
 * Json provides convenience functionality to parse and format JSON to/from fabric Values
 */
object Json extends AbstractJson {
  private lazy val factory = new JsonFactory()
    .enable(JsonParser.Feature.ALLOW_COMMENTS)
    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)

  override def parse(s: String): Value = {
    val parser = factory.createParser(s)
    try {
      parse(parser)
    } finally {
      parser.close()
    }
  }

  def parse(file: File): Value = parse(Source.fromFile(file, "UTF-8"))
  def parse(path: Path): Value = parse(Source.fromFile(path.toFile, "UTF-8"))

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