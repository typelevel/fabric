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

import fabric._
import fabric.define.{DefType, Definition}

import scala.collection.immutable.VectorMap

/**
  * The RW derived for an enum or sealed trait whose cases are all parameterless but one, marked `@unknownCase`. Every
  * declared case is written as a plain string, as it is when the type has no marked case; any other string decodes to
  * the marked case, which holds it in its one `String` field and is written back as that same string. The definition
  * names only the declared cases.
  *
  * @param named        each declared case with the string it is written as
  * @param className    the type's class name in its definition
  * @param unknown      the marked case's RW
  * @param unknownField the marked case's field
  */
class UnknownStringCaseRW[T](named: => List[(T, String)], className: String, unknown: => RW[T], unknownField: String)
    extends RW[T] {
  private lazy val instanceToName: Map[T, String] = named.toMap
  private lazy val nameToInstance: Map[String, T] = named.map(_.swap).toMap
  private lazy val leafIndex: Map[String, List[T]] =
    named.groupBy { case (_, name) => name.split('.').lastOption.getOrElse(name).toLowerCase }.view.mapValues(_.map(_._1)).toMap

  override def read(value: T): Json = instanceToName.get(value) match {
    case Some(name) => Str(name)
    case None =>
      unknown.read(value) match {
        case Obj(fields) if fields.contains(unknownField) => fields(unknownField)
        case other => throw RWException(s"Expected '$unknownField' in $value, got: $other")
      }
  }

  override def write(json: Json): T = json match {
    case Str(name, _) =>
      nameToInstance.get(name).orElse {
        leafIndex.get(name.toLowerCase) match {
          case Some(single :: Nil) => Some(single)
          case Some(many) =>
            throw RWException(
              s"Ambiguous legacy enumeration value '$name' — matches ${many.size} cases: " +
                s"[${many.map(instanceToName).mkString(", ")}]. Migrate the stored records to the new wire form."
            )
          case None => None
        }
      }.getOrElse(unknown.write(Obj((unknownField, Str(name)))))
    case _ => throw RWException(s"Expected string for enumeration, got: $json")
  }

  override lazy val definition: Definition = Definition(
    DefType.Poly(named.map { case (_, name) => (name, Definition(DefType.Null)) }.toMap.to(VectorMap)),
    className = Some(className)
  )
}
