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
import fabric.define.{DefType, Definition}

import scala.collection.immutable.VectorMap

/**
  * The runtime half of a case class's derived RW. The derivation gives its fields as tables (their names, their RWs,
  * which have defaults) and a few small functions, and the loops here write, read and describe every field, so the
  * code a derivation leaves in an app's output stays small however many fields the class has. The JSON written and
  * read is the same as that of the per-field code the derivation used to unroll.
  *
  * @param className the class's name in its definition
  * @param pathName the class's name in a field's error path, where it is not its className (otherwise null)
  * @param labels the fields' names, in the constructor's order
  * @param kinds one character per field: 'd' has a default, 'o' is an Option without one, 'r' is required
  * @param jsonWrapper whether the class is a JsonWrapper, whose `json` field takes the whole object when it is missing
  * @param fields the fields' RWs, made on first use (they may name the RW being made): either one per field, or the
  *               readers, then the writers, then the RWs, where a field's three are not the same
  * @param defaults field i's default, for the fields of kind 'd' (null where none has one)
  * @param create the class made from its fields' values
  * @param extend the fields' JSON with the @serialized members added, the @notSerialized fields taken out and
  *               `_generic` added, where the class has any of them (otherwise null)
  * @param describe the definition with the annotations' descriptions, formats, deprecations, constraints, generic
  *                 names and types, @serialized members and @notSerialized fields applied (null where none applies)
  */
class CaseClassRW[T](
  className: String,
  pathName: String,
  labels: Array[String],
  kinds: String,
  jsonWrapper: Boolean,
  fields: () => Array[AnyRef],
  defaults: Int => Any,
  create: Array[Any] => T,
  extend: (T, Map[String, Json]) => Map[String, Json],
  describe: Definition => Definition
) extends ClassRW[T] {
  private val path: String = if (pathName == null) className else pathName

  private lazy val codecs: Array[AnyRef] = fields()

  private def reader(i: Int): Reader[Any] = codecs(i).asInstanceOf[Reader[Any]]

  private def writer(i: Int): Writer[Any] = {
    val c = codecs
    c(if (c.length == labels.length) i else labels.length + i).asInstanceOf[Writer[Any]]
  }

  private def rw(i: Int): RW[Any] = {
    val c = codecs
    c(if (c.length == labels.length) i else 2 * labels.length + i).asInstanceOf[RW[Any]]
  }

  override protected def t2Map(t: T): Map[String, Json] = {
    val p = t.asInstanceOf[Product]
    val b = VectorMap.newBuilder[String, Json]
    var i = 0
    while (i < labels.length) {
      b += labels(i) -> reader(i).read(p.productElement(i))
      i += 1
    }
    if (extend == null) b.result() else extend(t, b.result())
  }

  override protected def map2T(map: Map[String, Json]): T = {
    val values = new Array[Any](labels.length)
    var i = 0
    while (i < labels.length) {
      values(i) = field(map, i)
      i += 1
    }
    create(values)
  }

  private def field(map: Map[String, Json], i: Int): Any = {
    val label = labels(i)
    CompileRW.findValueCaseInsensitive(map, label) match {
      case Some(json) => json match {
          case Null if kinds.charAt(i) == 'd' => defaults(i)
          case _ => RWFieldHelper.writeField(writer(i), json, path, label)
        }
      case None =>
        if (label == "json" && jsonWrapper) {
          writer(i).write(Obj(map))
        } else {
          kinds.charAt(i) match {
            case 'd' => defaults(i)
            case 'o' => None
            case _ => throw RWException(s"Unable to find field $path.$label (and no defaults set) in ${Obj(map)}")
          }
        }
    }
  }

  override lazy val definition: Definition = {
    val b = VectorMap.newBuilder[String, Definition]
    labels.indices.foreach(i => b += labels(i) -> rw(i).definition)
    // a default is read only once a consumer asks for it, so a stateful one (an id generator) is not run by the schema
    val lazyDefaults = labels.indices.collect {
      case i if kinds.charAt(i) == 'd' => labels(i) -> (() => reader(i).read(defaults(i)))
    }.toMap
    val d = Definition.applyFieldDefaultsLazy(Definition(DefType.Obj(b.result())), lazyDefaults).withClassName(className)
    if (describe == null) d else describe(d)
  }
}

object CaseClassRW {

  /** A class's fields' values, in the constructor's order, as the Product its Mirror makes the class from. */
  final class Values(values: Array[Any]) extends Product {
    override def canEqual(that: Any): Boolean = true
    override def productArity: Int = values.length
    override def productElement(n: Int): Any = values(n)
  }
}
