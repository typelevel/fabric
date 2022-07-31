package fabric.parse

import com.fasterxml.jackson.core.{JsonFactory, JsonParser => JParser, JsonToken}
import fabric.{Arr, Bool, Null, NumDec, NumInt, Obj, Str, Json}

import scala.annotation.tailrec

object JacksonParser extends Parser {
  private lazy val factory = new JsonFactory()
    .enable(JParser.Feature.ALLOW_COMMENTS)
    .enable(JParser.Feature.ALLOW_SINGLE_QUOTES)
    .enable(JParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    .enable(JParser.Feature.ALLOW_YAML_COMMENTS)

  override def parse(s: String): Json = {
    val parser = factory.createParser(s)
    try {
      parse(parser)
    } finally {
      parser.close()
    }
  }

  protected def parse(parser: JParser): Json = parseToken(parser, parser.nextToken())

  private def parseToken(parser: JParser, token: JsonToken): Json = token match {
    case JsonToken.START_OBJECT => parseObj(parser, Map.empty)
    case JsonToken.START_ARRAY => parseArr(parser, Nil)
    case JsonToken.VALUE_STRING => Str(parser.getValueAsString)
    case JsonToken.VALUE_NUMBER_FLOAT => NumDec(BigDecimal(parser.getValueAsDouble))
    case JsonToken.VALUE_NUMBER_INT => NumInt(parser.getValueAsLong)
    case JsonToken.VALUE_NULL => Null
    case JsonToken.VALUE_TRUE => Bool(true)
    case JsonToken.VALUE_FALSE => Bool(false)
    case t => throw new RuntimeException(s"Unsupported token: $t")
  }

  @tailrec
  private def parseObj(parser: JParser, map: Map[String, Json]): Obj = {
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
  private def parseArr(parser: JParser, list: List[Json]): Arr = {
    val next = parser.nextToken()
    if (next == JsonToken.END_ARRAY) {
      Arr(list.reverse.toVector)
    } else {
      val value = parseToken(parser, next)
      parseArr(parser, value :: list)
    }
  }
}