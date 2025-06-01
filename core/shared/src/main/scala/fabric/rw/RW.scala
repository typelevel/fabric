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

package fabric.rw

import fabric.*
import fabric.define.DefType

import scala.reflect.ClassTag

/**
  * RW provides a single class representation of a Reader and Writer for the
  * same type
  */
trait RW[T] extends Reader[T] with Writer[T] {
  def definition: DefType

  def withPreWrite(f: Json => Json): RW[T] = EnhancedRW[T](this, preWrite = List(f))
  def withPostRead(f: (T, Json) => Json): RW[T] = EnhancedRW[T](this, postRead = List(f))
}

object RW extends CompileRW {
  def from[T](r: T => Json, w: Json => T, d: => DefType): RW[T] = new RW[T] {
    override def write(value: Json): T = w(value)

    override def read(t: T): Json = r(t)

    override def definition: DefType = d
  }

  def enumeration[T: ClassTag](
    list: List[T],
    asString: T => String = (t: T) => defaultClassNameMapping(t.getClass.getName),
    caseSensitive: Boolean = false
  ): RW[T] = new RW[T] {
    val className: String = implicitly[ClassTag[T]].runtimeClass.getName

    private def fixString(s: String): String = if (caseSensitive) s else s.toLowerCase

    private lazy val map = list.map(t => fixString(asString(t)) -> t).toMap

    override def write(value: Json): T = map(fixString(value.asString))

    override def read(t: T): Json = str(asString(t))

    override val definition: DefType = DefType.Enum(list.map(t => Str(asString(t))), Some(className))
  }

  def string[T](asString: T => String, fromString: String => T): RW[T] = new RW[T] {
    override def write(value: Json): T = fromString(value.asString)

    override def read(t: T): Json = str(asString(t))

    override def definition: DefType = DefType.Str
  }

  def wrapped[T](key: String, asJson: T => Json, fromJson: Json => T, definition: DefType = DefType.Json): RW[T] =
    RW.from(
      r = t => obj(key -> asJson(t)),
      w = j => fromJson(j(key)),
      d = DefType.Obj(Some(key), key -> definition)
    )

  /**
    * Convenience functionality to provide a static / singleton value that
    * represents that type
    *
    * @param value
    *   the singleton value to use
    */
  def static[T](value: T): RW[T] = from(_ => obj(), _ => value, DefType.Obj(Some(cleanClassName(value.getClass.getName))))

  /**
    * Convenience functionality for working with polymorphic types
    *
    * @param fieldName
    *   the field name stored in the value (defaults to "type")
    * @param classNameMapping
    *   a function to convert from the actual class name to a stringified class name
    */
  def poly[P: ClassTag](
    fieldName: String = "type",
    classNameMapping: String => String = defaultClassNameMapping
  )(
    types: RW[_ <: P]*
  ): RW[P] = {
    def typeName(rw: RW[_ <: P]): String = {
      val className = rw.definition.className.getOrElse(throw new RuntimeException(s"No className defined for $rw"))
      classNameMapping(className)
    }
    val className: String = implicitly[ClassTag[P]].runtimeClass.getName
    val typeMap = Map(types.map(rw => typeName(rw).toLowerCase -> rw): _*)
    from(
      r = (p: P) => {
        val `type` = classNameMapping(p.getClass.getName)
        typeMap.get(`type`.toLowerCase) match {
          case Some(rw) => rw.asInstanceOf[RW[P]].read(p).merge(obj("type" -> str(`type`)))
          case None => throw new RuntimeException(
              s"Type not found [${`type`}] converting from value $p. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      w = (json: Json) => {
        val `type` = json(fieldName).asString.toLowerCase
        typeMap.get(`type`) match {
          case Some(rw) => rw.write(json)
          case None => throw new RuntimeException(
              s"Type not found [${`type`}] converting from value $json. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      d = DefType.Poly(
        types.map { rw =>
          val obj = rw.definition.asInstanceOf[DefType.Obj]
          val key = typeName(rw)
          key -> obj
        }.toMap,
        Some(className)
      )
    )
  }

  def cleanClassName(className: String): String = className.replace("$u0020", " ").replace("$", ".") match {
    case s if s.endsWith(".") => s.substring(0, s.length - 1)
    case s => s
  }

  def defaultClassNameMapping(className: String): String = {
    val cn = cleanClassName(className)
    val index = cn.lastIndexOf('.')
    if (index != -1) {
      cn.substring(index + 1)
    } else {
      cn
    }
  }.replace("$", "")
}
