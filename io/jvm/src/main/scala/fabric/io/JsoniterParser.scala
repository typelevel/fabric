/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.io

import com.jsoniter.JsonIterator
import fabric._

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

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
    case com.jsoniter.ValueType.NUMBER =>
      iterator.readBigDecimal() match {
        case bd if bd.scale() == 0 => NumInt(bd.longValue())
        case bd => NumDec(BigDecimal(bd))
      }
    case com.jsoniter.ValueType.BOOLEAN => Bool(iterator.readBoolean())
    case com.jsoniter.ValueType.OBJECT => readObj(iterator)
    case com.jsoniter.ValueType.STRING => Str(iterator.readString())
    case com.jsoniter.ValueType.INVALID =>
      throw new RuntimeException("Invalid!")
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
    var list = List.empty[(String, Json)]

    @tailrec
    def recurse(): Unit = Option(iterator.readObject()) match {
      case None => // Finished
      case Some(key) =>
        list = (key -> read(iterator)) :: list
        recurse()
    }
    recurse()

    Obj(ListMap.from(list.reverse))
  }
}
