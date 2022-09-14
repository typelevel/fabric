package fabric.io

object JsonParser extends MultiFormatParser {
  override def parsers: List[FormatParser] = List(JSJsonParser)
}