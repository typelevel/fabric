package fabric

import scala.collection.mutable

object FabricGenerator {
  def apply(dt: DefType,
            rootName: String,
            mappings: (String, String)*): GeneratedClass = {
    val name2ClassName = mappings.toMap
    var additional = List.empty[GeneratedClass]

    def generate(rootName: String, map: Map[String, DefType]): GeneratedClass = {
      def typeFor(name: String, dt: DefType): String = {
        dt match {
          case DefType.Obj(map) =>
            val className = name2ClassName(name)
            additional = generate(className, map) :: additional
            className
          case DefType.Arr(t) => s"Vector[${typeFor(name, t)}]"
          case DefType.Opt(t) => s"Option[${typeFor(name, t)}]"
          case DefType.Str => "String"
          case DefType.Int => "Long"
          case DefType.Dec => "BigDecimal"
          case DefType.Bool => "Boolean"
          case DefType.Null => throw new RuntimeException("Null type found in definition! Not supported for code generation!")
        }
      }

      val b = new mutable.StringBuilder
      val (packageName, className) = if (rootName.contains('.')) {
        val index = rootName.lastIndexOf('.')
        Some(rootName.substring(0, index)) -> rootName.substring(index + 1)
      } else {
        None -> rootName
      }
      packageName.foreach { n =>
        b.append(s"package $n\n\n")
      }
      b.append("import fabric.rw._\n\n")
      b.append(s"case class $className(")
      b.append(map.map {
        case (name, value) => s"$name: ${typeFor(name, value)}"
      }.mkString(", "))
      b.append(")\n\n")
      b.append(s"object $className {\n")
      b.append(s"  implicit val rw: RW[$className] = ccRW\n")
      b.append("}")
      GeneratedClass(packageName, className, b.toString(), additional.reverse)
    }

    dt match {
      case DefType.Obj(map) => generate(rootName, map)
      case _ => throw new RuntimeException(s"Only DefType.Obj is supported for generation, but received: $dt")
    }
  }
}