package fabric.io

import com.jsoniter.JsonIterator
import fabric._

import scala.annotation.tailrec

object JsoniterParser extends FormatParser {
  override def format: Format = Format.Json

  override def apply(content: String): Json = {
    val iterator = JsonIterator.parse(content)
    read(iterator)
  }

  private def read(iterator: JsonIterator): Json = iterator.whatIsNext() match {
    case com.jsoniter.ValueType.NULL =>
      iterator.readNull()
      Null
    case com.jsoniter.ValueType.ARRAY => readArr(iterator)
    case com.jsoniter.ValueType.NUMBER => iterator.readBigDecimal() match {
      case bd if bd.scale() == 0 => NumInt(bd.longValue())
      case bd => NumDec(BigDecimal(bd))
    }
    case com.jsoniter.ValueType.BOOLEAN => Bool(iterator.readBoolean())
    case com.jsoniter.ValueType.OBJECT => readObj(iterator)
    case com.jsoniter.ValueType.STRING => Str(iterator.readString())
    case com.jsoniter.ValueType.INVALID => throw new RuntimeException("Invalid!")
  }

  private def readArr(iterator: JsonIterator): Json = {
    var list = List.empty[Json]

    @tailrec
    def recurse(): Unit = if (!iterator.readArray()) {
      // Finished
    } else {
      list = read(iterator) :: list
      recurse()
    }

    recurse()
    Arr(list.reverse.toVector)
  }

  private def readObj(iterator: JsonIterator): Json = {
    var map = Map.empty[String, Json]

    @tailrec
    def recurse(): Unit = Option(iterator.readObject()) match {
      case None => // Finished
      case Some(key) =>
        map += key -> read(iterator)
        recurse()
    }
    recurse()

    Obj(map)
  }
}