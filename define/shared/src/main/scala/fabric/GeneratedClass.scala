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

import java.io.{File, FileOutputStream}

case class GeneratedClass(
    packageName: Option[String],
    className: String,
    code: String,
    additional: List[GeneratedClass]
) {
  private val written = new ThreadLocal[Set[String]] {
    override def initialValue(): Set[String] = Set.empty
  }

  def write(baseDirectory: File, writeAdditional: Boolean = true): Unit = {
    val alreadyWritten = written.get()
    val fullName = packageName.map(p => s"$p.$className").getOrElse(className)
    if (alreadyWritten.contains(fullName)) {
      // Already written
    } else {
      val directory = packageName match {
        case Some(p) => new File(baseDirectory, p.replace('.', '/'))
        case None => baseDirectory
      }
      directory.mkdirs()
      val file = new File(directory, s"$className.scala")
      val io = new FileOutputStream(file)
      try {
        io.write(code.getBytes("UTF-8"))
        io.flush()
      } finally {
        io.close()
      }
      written.set(alreadyWritten + fullName)
      if (writeAdditional) additional.foreach(_.write(baseDirectory))
    }
  }
}
