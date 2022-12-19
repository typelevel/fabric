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

package fabric.define

import scala.collection.mutable

object FabricGenerator {
  def withMappings(
      dt: DefType,
      rootName: String,
      mappings: (String, String)*
  ): GeneratedClass = {
    val map = mappings.toMap
    apply(dt, rootName, map.apply)
  }

  def apply(
      dt: DefType,
      rootName: String,
      resolver: String => String
  ): GeneratedClass = {
    var additional = List.empty[GeneratedClass]

    def generate(
        rootName: String,
        original: Map[String, DefType]
    ): GeneratedClass = {
      val map = original.filterNot {
        case (_, DefType.Null) => true
        case (_, DefType.Arr(DefType.Null)) => true
        case _ => false
      }
      def typeFor(name: String, dt: DefType): String = dt match {
        case DefType.Obj(map) =>
          val className = resolver(name)
          additional = generate(className, map) :: additional
          className
        case DefType.Arr(t) => s"Vector[${typeFor(name, t)}]"
        case DefType.Opt(t) => s"Option[${typeFor(name, t)}]"
        case DefType.Str => "String"
        case DefType.Int => "Long"
        case DefType.Dec => "BigDecimal"
        case DefType.Bool => "Boolean"
        case DefType.Enum(_) => throw new RuntimeException("Unsupported")
        case DefType.Null =>
          throw new RuntimeException(
            "Null type found in definition! Not supported for code generation!"
          )
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
      def fixName(name: String): String = name match {
        case "type" => "`type`"
        case "private" => "`private`"
        case _ if name.contains('+') | name.contains('-') => s"`$name`"
        case _ => name
      }
      b.append(
        map
          .map { case (name, value) =>
            s"${fixName(name)}: ${typeFor(name, value)}"
          }
          .mkString(", ")
      )
      b.append(")\n\n")
      b.append(s"object $className {\n")
      b.append(s"  implicit val rw: RW[$className] = RW\n")
      b.append("}")
      GeneratedClass(packageName, className, b.toString(), additional.reverse)
    }

    dt match {
      case DefType.Obj(map) => generate(rootName, map)
      case _ =>
        throw new RuntimeException(
          s"Only DefType.Obj is supported for generation, but received: $dt"
        )
    }
  }
}
