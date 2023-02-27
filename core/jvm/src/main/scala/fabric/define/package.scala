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
import java.nio.file.Files

package object define {
  private val written = new ThreadLocal[Set[String]] {
    override def initialValue(): Set[String] = Set.empty
  }

  implicit class GeneratedClassExtras(gc: GeneratedClass) {
    def write(
      baseDirectory: File,
      writeAdditional: Boolean = true,
      validate: Boolean = false
    ): Unit = {
      val alreadyWritten = written.get()
      val fullName = gc.packageName.map(p => s"$p.${gc.className}").getOrElse(gc.className)
      if (alreadyWritten.contains(fullName)) {
        // Already written
      } else {
        val directory = gc.packageName match {
          case Some(p) => new File(baseDirectory, p.replace('.', '/'))
          case None => baseDirectory
        }
        directory.mkdirs()
        val file = new File(directory, s"${gc.className}.scala")
        if (file.isFile) {
          val existing = new String(Files.readAllBytes(file.toPath), "UTF-8")
          if (gc.code != existing) {
            val msg =
              s"Validation failed for existing file: ${gc.className} attempting to write modified code:\n${gc.code}\nOriginal:\n$existing"
            if (validate) {
              throw new RuntimeException(msg)
            } else {
              println(msg)
            }
          }
        }
        val io = new FileOutputStream(file)
        try {
          io.write(gc.code.getBytes("UTF-8"))
          io.flush()
        } finally io.close()
        written.set(alreadyWritten + fullName)
        if (writeAdditional) {
          gc.additional.foreach(
            _.write(baseDirectory, writeAdditional, validate)
          )
        }
      }
    }
  }
}
