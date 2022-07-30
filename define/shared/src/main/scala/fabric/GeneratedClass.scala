package fabric

import java.io.{File, FileOutputStream}

case class GeneratedClass(packageName: Option[String], className: String, code: String, additional: List[GeneratedClass]) {
  def write(baseDirectory: File, writeAdditional: Boolean = true): Unit = {
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
    if (writeAdditional) additional.foreach(_.write(baseDirectory))
  }
}