package fabric

import fabric.rw.{Asable, ReaderWriter}

package object io {
  implicit class StringIOExtras(s: String) {
    def as[T: ReaderWriter](format: Format): T = {
      val json: Json = JsonParser(s, format)
      json.as[T]
    }
  }

  implicit class ByteArrayIOExtras(array: Array[Byte]) {
    def as[T: ReaderWriter](format: Format): T = {
      val json: Json = JsonParser(array, format)
      json.as[T]
    }
  }
}