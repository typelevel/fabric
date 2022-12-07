package fabric

import java.io.{File, FileOutputStream}

package object define {
  implicit class GeneratedClassExtras(gc: GeneratedClass) {
    private val written = new ThreadLocal[Set[String]] {
      override def initialValue(): Set[String] = Set.empty
    }

    def write(baseDirectory: File, writeAdditional: Boolean = true): Unit = {
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
        val io = new FileOutputStream(file)
        try {
          io.write(gc.code.getBytes("UTF-8"))
          io.flush()
        } finally {
          io.close()
        }
        written.set(alreadyWritten + fullName)
        if (writeAdditional) gc.additional.foreach(_.write(baseDirectory))
      }
    }
  }
}