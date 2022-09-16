package fabric.io

import com.fasterxml.jackson.core.{JsonFactory, JsonToken, JsonParser => JParser}
import fabric.{Arr, Bool, Json, Null, NumDec, NumInt, Obj, Str}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object JacksonParser extends FormatParser {
  private lazy val factory = new JsonFactory()
    .enable(JParser.Feature.ALLOW_COMMENTS)
    .enable(JParser.Feature.ALLOW_SINGLE_QUOTES)
    .enable(JParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    .enable(JParser.Feature.ALLOW_YAML_COMMENTS)

  override def format: Format = Format.Json

  override def apply(content: String): Json = {
    val parser = factory.createParser(content)
    try {
      parse(parser)
    } finally {
      parser.close()
    }
  }

  protected def parse(parser: JParser): Json = parseToken(parser, parser.nextToken())

  private def parseToken(parser: JParser, token: JsonToken): Json = token match {
    case JsonToken.START_OBJECT => parseObj(parser, ListMap.empty)
    case JsonToken.START_ARRAY => parseArr(parser, Vector.empty)
    case JsonToken.VALUE_STRING => Str(parser.getValueAsString)
    case JsonToken.VALUE_NUMBER_FLOAT => NumDec(BigDecimal(parser.getValueAsDouble))
    case JsonToken.VALUE_NUMBER_INT => NumInt(parser.getValueAsLong)
    case JsonToken.VALUE_NULL => Null
    case JsonToken.VALUE_TRUE => Bool(true)
    case JsonToken.VALUE_FALSE => Bool(false)
    case t => throw new RuntimeException(s"Unsupported token: $t")
  }

  @tailrec
  private def parseObj(parser: JParser, map: ListMap[String, Json]): Obj = {
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
  private def parseArr(parser: JParser, vector: Vector[Json]): Arr = {
    val next = parser.nextToken()
    if (next == JsonToken.END_ARRAY) {
      Arr(vector)
    } else {
      val value = parseToken(parser, next)
      parseArr(parser, vector :+ value)
    }
  }
}