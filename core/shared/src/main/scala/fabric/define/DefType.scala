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

import fabric._
import fabric.rw.RW

import scala.collection.immutable.VectorMap
import scala.util.Try

sealed trait DefType {
  def isOpt: Boolean = false

  def isNull: Boolean = false

  def validate(value: Json): Boolean = Try(FabricDefinition(value).merge(this)).toOption.contains(this)

  def opt: DefType = DefType.Opt(this)

  final def template(config: TemplateConfig): Json = template(JsonPath.empty, config)

  protected def template(path: JsonPath, config: TemplateConfig): Json

  def merge(that: DefType): DefType =
    if (this == that) {
      this
    } else if (this.isNull) {
      that.opt
    } else if (that.isNull) {
      this.opt
    } else if (this.opt == that) {
      that
    } else if (this == that.opt) {
      this
    } else {
      throw new RuntimeException(s"Incompatible typed:\n$this\n\n$that")
    }
}

object DefType {
  implicit def rw: RW[DefType] = RW.from[DefType](r = dt2V, w = v2dt, d = DefType.Dynamic)

  private def dt2V(dt: DefType): Json = dt match {
    case Obj(map) => obj(
        "type" -> "object",
        "values" -> fabric.Obj(map.map { case (key, dt) => key -> dt2V(dt) })
      )
    case Arr(t) => obj("type" -> "array", "value" -> dt2V(t))
    case Opt(t) => obj("type" -> "optional", "value" -> dt2V(t))
    case Str => obj("type" -> "string")
    case Int => obj("type" -> "numeric", "precision" -> "integer")
    case Dec => obj("type" -> "numeric", "precision" -> "decimal")
    case Bool => obj("type" -> "boolean")
    case Enum(values) => obj("type" -> "enum", "values" -> values)
    case Poly(values) => obj(
        "type" -> "poly",
        "values" -> values.map { case (key, dt) => key -> dt2V(dt) }
      )
    case Dynamic => obj("type" -> "dynamic")
    case Null => obj("type" -> "null")
  }

  private def v2dt(v: Json): DefType = {
    val o = v.asObj
    o.value("type").asString match {
      case "object" => Obj(o.value("values").asMap.map { case (key, value) => key -> v2dt(value) })
      case "array" => Arr(v2dt(o.value("value")))
      case "optional" => Opt(v2dt(o.value("value")))
      case "string" => Str
      case "numeric" => o.value("precision").asString match {
          case "integer" => Int
          case "decimal" => Dec
        }
      case "boolean" => Bool
      case "enum" => Enum(o.value("values").asVector.toList)
      case "poly" => Poly(o.value("values").asMap.map { case (key, json) => key -> v2dt(json) })
      case "dynamic" => Dynamic
      case "null" => Null
    }
  }

  case class Obj(map: Map[String, DefType]) extends DefType {
    override def merge(that: DefType): DefType = that match {
      case Obj(thatMap) => Obj(mergeMap(map, thatMap))
      case Opt(Obj(thatMap)) => Opt(Obj(mergeMap(map, thatMap)))
      case _ => super.merge(that)
    }

    private def mergeMap(
      m1: Map[String, DefType],
      m2: Map[String, DefType]
    ): Map[String, DefType] = {
      val keys = m1.keySet ++ m2.keySet
      VectorMap(keys.toList.map(key => key -> m1.getOrElse(key, Null).merge(m2.getOrElse(key, Null))): _*)
    }

    override def template(path: JsonPath, config: TemplateConfig): Json = obj(map.toList.map { case (key, dt) =>
      key -> dt.template(path \ key, config)
    }: _*)
  }
  object Obj {
    def apply(entries: (String, DefType)*): Obj = Obj(VectorMap(entries: _*))
  }
  case class Arr(t: DefType) extends DefType {
    override def merge(that: DefType): DefType = that match {
      case Arr(thatType) => Arr(t.merge(thatType))
      case Null => this
      case _ => super.merge(that)
    }

    override def template(path: JsonPath, config: TemplateConfig): Json = arr(
      t.template(path \ 0, config),
      t.template(path \ 1, config),
      t.template(path \ 2, config)
    )
  }
  case class Opt(t: DefType) extends DefType {
    override def isOpt: Boolean = true
    override def opt: DefType = this

    override def merge(that: DefType): DefType = that match {
      case Opt(thatOpt) => t.merge(thatOpt) match {
          case o: Opt => o
          case result => Opt(result)
        }
      case o: Obj => o.merge(t) match {
          case o: Opt => o
          case result => Opt(result)
        }
      case _ => super.merge(that)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = t.template(path, config)
  }
  case object Str extends DefType {
    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.string(path)
  }
  case object Int extends DefType {
    override def merge(that: DefType): DefType = that match {
      case DefType.Dec => that
      case _ => super.merge(that)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.int(path)
  }
  case object Dec extends DefType {
    override def merge(that: DefType): DefType = that match {
      case DefType.Int => this
      case _ => super.merge(that)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.dec(path)
  }
  case object Bool extends DefType {
    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.bool(path)
  }
  case object Dynamic extends DefType {
    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.dynamic(path)
  }
  case class Enum(values: List[Json]) extends DefType {
    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.`enum`(path, values)
  }
  case class Poly(values: Map[String, DefType]) extends DefType {
    override protected def template(path: JsonPath, config: TemplateConfig): Json = values.head._2.template(path, config)
  }
  case object Null extends DefType {
    override def isNull: Boolean = true

    override def merge(that: DefType): DefType = that match {
      case o: Opt => o
      case Null => Null
      case _ => that.merge(Null)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = fabric.Null
  }
}
