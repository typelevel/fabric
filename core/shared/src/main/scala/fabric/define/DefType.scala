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
import fabric.rw._

import scala.collection.immutable.VectorMap
import scala.util.Try

sealed trait DefType {
  def className: Option[String]

  def description: Option[String] = None

  def describe(desc: String): DefType = DefType.Described(this, Some(desc))

  def withClassName(cn: String): DefType = this match {
    case DefType.Classed(dt, _) => DefType.Classed(dt, cn)
    case _ => DefType.Classed(this, cn)
  }

  def isOpt: Boolean = false

  def isNull: Boolean = false

  def validate(value: Json): Boolean = Try(FabricDefinition(value).merge(this)).toOption.contains(this)

  def opt: DefType = DefType.Opt(this)

  final def template(config: TemplateConfig): Json = template(JsonPath.empty, config)

  protected def template(path: JsonPath, config: TemplateConfig): Json

  def merge(that: DefType): DefType = {
    // Unwrap Described for comparison, descriptions are not preserved through merge
    val unwrappedThis = this match { case DefType.Described(dt, _) => dt; case _ => this }
    val unwrappedThat = that match { case DefType.Described(dt, _) => dt; case _ => that }
    if (unwrappedThis != this || unwrappedThat != that) {
      unwrappedThis.merge(unwrappedThat)
    } else if (this == that) {
      this
    } else if (this.isNull) {
      that.opt
    } else if (that.isNull) {
      this.opt
    } else if (this.opt == that) {
      that
    } else if (this == that.opt) {
      this
    } else if (this.isOpt || that.isOpt) {
      DefType.Opt(DefType.Str) // Handle generic Optional
    } else if (this == DefType.Str || that == DefType.Str && !this.isOpt && !that.isOpt) {
      DefType.Str
    } else {
      throw new RuntimeException(s"Incompatible typed:\n$this\n\n$that")
    }
  }
}

object DefType {
  implicit def rw: RW[DefType] = RW.from[DefType](r = dt2V, w = v2dt, d = DefType.Json)

  private def withDesc(base: fabric.Obj, desc: Option[String]): fabric.Obj = desc match {
    case Some(d) => base.merge(fabric.Obj("description" -> str(d))).asObj
    case None => base
  }

  private def dt2V(dt: DefType): Json = dt match {
    case Described(inner, desc) => withDesc(dt2V(inner).asObj, desc)
    case Classed(inner, cn) =>
      val base = dt2V(inner).asObj
      base.merge(fabric.Obj("className" -> str(cn))).asObj
    case Obj(map, cn, desc) => withDesc(
        obj(
          "type" -> str("object"),
          "values" -> fabric.Obj(map.map { case (key, dt) => key -> dt2V(dt) }),
          "className" -> cn.json
        ),
        desc
      )
    case Arr(t, desc) => withDesc(obj("type" -> str("array"), "value" -> dt2V(t)), desc)
    case Opt(t, desc) => withDesc(obj("type" -> str("optional"), "value" -> dt2V(t)), desc)
    case Str => obj("type" -> str("string"))
    case Int => obj("type" -> str("numeric"), "precision" -> str("integer"))
    case Dec => obj("type" -> str("numeric"), "precision" -> str("decimal"))
    case Bool => obj("type" -> str("boolean"))
    case Enum(values, cn, desc) => withDesc(obj("type" -> str("enum"), "values" -> values, "className" -> cn.json), desc)
    case Poly(values, cn, desc) => withDesc(
        obj(
          "type" -> str("poly"),
          "values" -> values.map { case (key, dt) => key -> dt2V(dt) },
          "className" -> cn.json
        ),
        desc
      )
    case Json => obj("type" -> str("json"))
    case Null => obj("type" -> str("null"))
  }

  private def readDesc(o: fabric.Obj): Option[String] = o.get("description").map(_.asString)

  private def withClass(dt: DefType, o: fabric.Obj): DefType = o.get("className").map(_.asString) match {
    case Some(cn) => Classed(dt, cn)
    case None => dt
  }

  private def v2dt(v: Json): DefType = {
    val o = v.asObj
    val desc = readDesc(o)
    o.value("type").asString match {
      case "object" =>
        val map: Map[String, Json] = o.value
        val cnOption: Option[Json] = map.get("className")
        val cn: Json = cnOption.getOrElse(fabric.Null)
        Obj(
          map = o.value("values").asMap.map { case (key, value) => key -> v2dt(value) },
          className = cn match {
            case fabric.Null => None
            case s: Str => Some(s.value)
            case j => throw new RuntimeException(s"Unsupported className value: $j")
          },
          description = desc
        )
      case "array" => Arr(t = v2dt(o.value("value")), description = desc)
      case "optional" => Opt(v2dt(o.value("value")), description = desc)
      case "string" => withClass(desc.fold[DefType](Str)(Str.describe), o)
      case "numeric" => o.value("precision").asString match {
          case "integer" => withClass(desc.fold[DefType](Int)(Int.describe), o)
          case "decimal" => withClass(desc.fold[DefType](Dec)(Dec.describe), o)
        }
      case "boolean" => withClass(desc.fold[DefType](Bool)(Bool.describe), o)
      case "enum" => Enum(o.value("values").asVector.toList, o.get("className").map(_.asString), description = desc)
      case "poly" => Poly(
          o.value("values").asMap.map { case (key, json) => key -> v2dt(json) },
          o.get("className").map(_.asString),
          description = desc
        )
      case "json" => withClass(desc.fold[DefType](Json)(Json.describe), o)
      case "null" => withClass(desc.fold[DefType](Null)(Null.describe), o)
    }
  }

  case class Obj(map: Map[String, DefType], className: Option[String], override val description: Option[String] = None)
      extends DefType {
    override def describe(desc: String): Obj = copy(description = Some(desc))

    override def merge(that: DefType): DefType = that match {
      case Obj(thatMap, cn, _) => Obj(mergeMap(map, thatMap), cn)
      case Opt(Obj(thatMap, cn, _), _) => Opt(Obj(mergeMap(map, thatMap), cn))
      case _ => super.merge(that)
    }

    private def mergeMap(
      m1: Map[String, DefType],
      m2: Map[String, DefType]
    ): Map[String, DefType] = {
      val keys = m1.keySet ++ m2.keySet
      VectorMap(keys.toList.map(key => key -> m1.getOrElse(key, Null).merge(m2.getOrElse(key, Null)))*)
    }

    override def template(path: JsonPath, config: TemplateConfig): Json = obj(map.toList.map { case (key, dt) =>
      key -> dt.template(path \ key, config)
    }*)
  }
  object Obj {
    def apply(className: Option[String], entries: (String, DefType)*): Obj = Obj(VectorMap(entries*), className)
  }
  case class Arr(t: DefType, override val description: Option[String] = None) extends DefType {
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(this, className)
    override def describe(desc: String): Arr = copy(description = Some(desc))

    override def merge(that: DefType): DefType = that match {
      case Arr(thatType, _) => Arr(t.merge(thatType))
      case Null => this
      case _ => super.merge(that)
    }

    override def template(path: JsonPath, config: TemplateConfig): Json = arr(
      t.template(path \ 0, config),
      t.template(path \ 1, config),
      t.template(path \ 2, config)
    )
  }
  case class Opt(t: DefType, override val description: Option[String] = None) extends DefType {
    override def className: Option[String] = Some("scala.Option")
    override def isOpt: Boolean = true
    override def opt: DefType = this
    override def describe(desc: String): Opt = copy(description = Some(desc))

    override def merge(that: DefType): DefType = that match {
      case Opt(thatOpt, _) => t.merge(thatOpt) match {
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
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(Str, className)

    override protected def template(path: JsonPath, config: TemplateConfig): Json = str(config.string(path))
  }
  case object Int extends DefType {
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(Int, className)

    override def merge(that: DefType): DefType = that match {
      case DefType.Dec => that
      case _ => super.merge(that)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = num(config.int(path))
  }
  case object Dec extends DefType {
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(Dec, className)

    override def merge(that: DefType): DefType = that match {
      case DefType.Int => this
      case _ => super.merge(that)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = num(config.dec(path))
  }
  case object Bool extends DefType {
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(Bool, className)

    override protected def template(path: JsonPath, config: TemplateConfig): Json = bool(config.bool(path))
  }
  case object Json extends DefType {
    override def className: Option[String] = None
    def apply(className: String): DefType = Classed(Json, className)

    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.json(path)
  }
  case class Enum(values: List[Json], className: Option[String], override val description: Option[String] = None)
      extends DefType {
    override def describe(desc: String): Enum = copy(description = Some(desc))
    override protected def template(path: JsonPath, config: TemplateConfig): Json = config.`enum`(path, values)
  }
  case class Poly(
    values: Map[String, DefType],
    className: Option[String],
    override val description: Option[String] = None
  ) extends DefType {
    override def describe(desc: String): Poly = copy(description = Some(desc))
    override protected def template(path: JsonPath, config: TemplateConfig): Json = values.head._2.template(path, config)
  }
  case class Described(dt: DefType, override val description: Option[String]) extends DefType {
    override def className: Option[String] = dt.className
    override def isOpt: Boolean = dt.isOpt
    override def isNull: Boolean = dt.isNull
    override def opt: DefType = Described(dt.opt, description)
    override def describe(desc: String): Described = copy(description = Some(desc))
    override def merge(that: DefType): DefType = dt.merge(that)
    override protected def template(path: JsonPath, config: TemplateConfig): Json = dt.template(path, config)
  }
  case class Classed(dt: DefType, cn: String) extends DefType {
    override def className: Option[String] = Some(cn)
    override def description: Option[String] = dt.description
    override def isOpt: Boolean = dt.isOpt
    override def isNull: Boolean = dt.isNull
    override def withClassName(cn: String): DefType = copy(cn = cn)
    override def describe(desc: String): DefType = Classed(dt.describe(desc), cn)
    override def merge(that: DefType): DefType = Classed(dt.merge(that), cn)
    override protected def template(path: JsonPath, config: TemplateConfig): Json = dt.template(path, config)
  }
  case object Null extends DefType {
    override def className: Option[String] = None

    override def isNull: Boolean = true

    override def merge(that: DefType): DefType = that match {
      case o: Opt => o
      case Null => Null
      case _ => that.merge(Null)
    }

    override protected def template(path: JsonPath, config: TemplateConfig): Json = fabric.Null
  }
}
