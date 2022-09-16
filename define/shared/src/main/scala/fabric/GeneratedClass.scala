package fabric

import java.io.{File, FileOutputStream}

case class GeneratedClass(packageName: Option[String], className: String, code: String, additional: List[GeneratedClass]) {
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