package fabric.define

case class GeneratedClass(packageName: Option[String],
                          className: String,
                          code: String,
                          additional: List[GeneratedClass])