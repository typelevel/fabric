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

package fabric

import java.nio.ByteBuffer
import scala.collection.immutable.VectorMap

object Cryo {
  private object identifiers {
    val Obj: Byte = 1
    val Str: Byte = 2
    val NumInt: Byte = 3
    val NumDec: Byte = 4
    val Bool: Byte = 5
    val Arr: Byte = 6
    val Null: Byte = -1
  }
  private object bytes {
    val Byte: Int = 1
    val Integer: Int = 4
    val Long: Int = 8
  }

  def bytes(json: Json): Int = json match {
    case Obj(map) => bytes.Byte + bytes.Integer + map.foldLeft(0)((sum, t) =>
        t match {
          case (key, value) => sum + bytes(Str(key)) + bytes(value)
        }
      )
    case Str(s) => bytes.Byte + bytes.Integer + s.length
    case NumInt(_) => bytes.Byte + bytes.Long
    case NumDec(bd) => bytes.Byte + bytes(Str(bd.toString()))
    case Bool(_) => bytes.Byte + bytes.Byte
    case Arr(v) => bytes.Byte + bytes.Integer + v.foldLeft(0)((sum, json) => sum + bytes(json))
    case Null => bytes.Byte
  }

  def freeze(json: Json, allocateDirect: Boolean): ByteBuffer = {
    val size = bytes(json)
    val bb =
      if (allocateDirect) ByteBuffer.allocateDirect(size)
      else ByteBuffer.allocate(size)
    freeze(json, bb)
    bb
  }

  def freeze(json: Json, bb: ByteBuffer): Unit = json match {
    case Obj(map) =>
      bb.put(identifiers.Obj)
      bb.putInt(map.size)
      map.foreach { case (key, value) =>
        freeze(Str(key), bb)
        freeze(value, bb)
      }
    case Str(s) =>
      bb.put(identifiers.Str)
      bb.putInt(s.length)
      bb.put(s.getBytes("UTF-8"))
      ()
    case NumInt(l) =>
      bb.put(identifiers.NumInt)
      bb.putLong(l)
      ()
    case NumDec(bd) =>
      bb.put(identifiers.NumDec)
      freeze(Str(bd.toString()), bb)
    case Bool(b) =>
      bb.put(identifiers.Bool)
      bb.put(if (b) 1.toByte else 0.toByte)
      ()
    case Arr(v) =>
      bb.put(identifiers.Arr)
      bb.putInt(v.length)
      v.foreach(json => freeze(json, bb))
    case Null =>
      bb.put(identifiers.Null)
      ()
  }

  def thaw(bb: ByteBuffer): Json = bb.get() match {
    case identifiers.Obj =>
      val size = bb.getInt
      val map = VectorMap((0 until size).map { _ =>
        val key = thaw(bb).asString
        val value = thaw(bb)
        key -> value
      }: _*)
      Obj(map)
    case identifiers.Str =>
      val size = bb.getInt
      val array = new Array[Byte](size)
      bb.get(array)
      Str(new String(array, "UTF-8"))
    case identifiers.NumInt => NumInt(bb.getLong)
    case identifiers.NumDec =>
      val s = thaw(bb).asString
      NumDec(BigDecimal(s))
    case identifiers.Bool => Bool(bb.get() == 1.toByte)
    case identifiers.Arr =>
      val size = bb.getInt
      val v = (0 until size).toVector.map(_ => thaw(bb))
      Arr(v)
    case identifiers.Null => Null
  }
}
