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

/**
  * Definition is the metadata-carrying wrapper around a [[DefType]]. While DefType represents the pure structural JSON
  * type (string, int, object, etc.), Definition adds:
  *
  *   - '''className''' — the fully-qualified Scala class name (e.g. `"com.example.Person"`)
  *   - '''description''' — an optional human-readable description (populated from `@description` annotations)
  *   - '''genericTypes''' — the resolved type parameters of a generic class (e.g. for `Wrapper[String]`, this contains
  *     `List(GenericType("T", Definition(DefType.Str)))`)
  *   - '''genericName''' — when this Definition is a field of a generic class, indicates which type parameter the field
  *     references (e.g. for `case class Wrapper[T](value: T)`, the `value` field has `genericName = Some("T")`)
  *   - '''format''' — the semantic format of the value (e.g. `Format.Email`, `Format.DateTime`), populated from
  *     `@format` annotations. Defaults to `Format.Raw` (no specific format).
  *   - '''defaultValue''' — the default value for this field as JSON, populated from case class default parameters
  *   - '''deprecated''' — whether this field/type is deprecated, populated from `@fieldDeprecated` annotations
  *
  * Definition is the return type of `RW[T].definition` and the primary type used for schema introspection, OpenAPI
  * generation, and code generation.
  *
  * ==Relationship to DefType==
  *
  * [[DefType]] is the "what type is it?" — a pure enum of JSON-compatible types. Definition is the "what do we know
  * about it?" — the full picture including the type, its Scala class identity, documentation, and generic type
  * information.
  *
  * Composite DefType variants (`Obj`, `Arr`, `Opt`, `Poly`) reference Definition for their inner types, not DefType
  * directly. This means every level of nesting carries its own metadata.
  *
  * ==Generic type tracking==
  *
  * For a generic class like `case class Wrapper[T](name: String, value: T, other: Option[T])`, when `RW.gen` is
  * invoked for `Wrapper[String]`, the resulting Definition has:
  *
  *   - `genericTypes = List(GenericType("T", Definition(DefType.Str)))` — records that `T` was resolved to `String`
  *   - Field `name` has `genericName = None` — not derived from a type parameter
  *   - Field `value` has `genericName = Some("T")` — this field's type comes from type parameter `T`
  *   - Field `other` has `genericName = Some("T")` — this field also references `T` (inside an `Option`)
  *
  * This information enables schema generators (e.g. OpenAPI) to emit proper generic type references rather than
  * inlining the resolved type.
  *
  * @param defType
  *   the structural JSON type
  * @param className
  *   optional fully-qualified Scala class name
  * @param description
  *   optional human-readable description
  * @param genericTypes
  *   resolved type parameters of this class (empty for non-generic types)
  * @param genericName
  *   when this Definition is a field, the name of the type parameter it references (e.g. `"T"`)
  * @param format
  *   the semantic format of the value (defaults to `Format.Raw`)
  * @param defaultValue
  *   the default value for this field as JSON, if one exists
  * @param deprecated
  *   whether this field/type is deprecated
  *
  * @see [[DefType]] for the pure type structure
  * @see [[GenericType]] for type parameter entries
  * @see [[Format]] for available format values
  */
case class Definition(
  defType: DefType,
  className: Option[String] = None,
  description: Option[String] = None,
  genericTypes: List[GenericType] = Nil,
  genericName: Option[String] = None,
  format: Format = Format.Raw,
  defaultValue: Option[Json] = None,
  deprecated: Boolean = false,
  constraints: Constraints = Constraints.Empty
) {

  /**
    * Returns true if the underlying type is optional (`DefType.Opt`).
    */
  def isOpt: Boolean = defType.isOpt

  /**
    * Returns true if the underlying type is null (`DefType.Null`).
    */
  def isNull: Boolean = defType.isNull

  /**
    * Wraps this Definition in `DefType.Opt` if not already optional.
    */
  def opt: Definition = if (isOpt) this else Definition(DefType.Opt(this))

  /**
    * Returns a copy with the given description.
    */
  def describe(desc: String): Definition = copy(description = Some(desc))

  /**
    * Returns a copy with the given class name.
    */
  def withClassName(cn: String): Definition = copy(className = Some(cn))

  /**
    * Validates that a JSON value conforms to this Definition's type structure.
    */
  def validate(value: Json): Boolean = Try(FabricDefinition(value).merge(this)).toOption.contains(this)

  /**
    * Generates a template JSON value from this Definition's type structure using the given config.
    */
  final def template(config: TemplateConfig): Json = template(JsonPath.empty, config)

  protected[define] def template(path: JsonPath, config: TemplateConfig): Json = defType match {
    case DefType.Obj(map) => obj(map.toList.map { case (key, d) => key -> d.template(path \ key, config) }*)
    case DefType.Arr(t) => arr(
        t.template(path \ 0, config),
        t.template(path \ 1, config),
        t.template(path \ 2, config)
      )
    case DefType.Opt(t) => t.template(path, config)
    case DefType.Str => str(config.string(path))
    case DefType.Int => num(config.int(path))
    case DefType.Dec => num(config.dec(path))
    case DefType.Bool => bool(config.bool(path))
    case DefType.Json => config.json(path)
    case DefType.Null => Null
    case DefType.Poly(values) => config.poly(path, values)
  }

  /**
    * Merges this Definition with another, combining their type structures. Used by [[FabricDefinition]] when inferring
    * types from multiple JSON values.
    */
  def merge(that: Definition): Definition = {
    val thisType = this.defType
    val thatType = that.defType
    (thisType, thatType) match {
      case _ if thisType == thatType => this
      case (DefType.Null, _) => that.opt
      case (_, DefType.Null) => this.opt
      case (DefType.Obj(m1), DefType.Obj(m2)) =>
        val keys = m1.keySet ++ m2.keySet
        val merged = VectorMap(keys.toList.map { key =>
          key -> m1.getOrElse(key, Definition(DefType.Null)).merge(m2.getOrElse(key, Definition(DefType.Null)))
        }*)
        that.copy(defType = DefType.Obj(merged))
      case (DefType.Obj(m1), DefType.Opt(inner @ Definition(DefType.Obj(m2), _, _, _, _, _, _, _, _))) =>
        val keys = m1.keySet ++ m2.keySet
        val merged = VectorMap(keys.toList.map { key =>
          key -> m1.getOrElse(key, Definition(DefType.Null)).merge(m2.getOrElse(key, Definition(DefType.Null)))
        }*)
        Definition(DefType.Opt(inner.copy(defType = DefType.Obj(merged))))
      case (DefType.Arr(t1), DefType.Arr(t2)) => Definition(DefType.Arr(t1.merge(t2)))
      case (DefType.Int, DefType.Dec) => that
      case (DefType.Dec, DefType.Int) => this
      case (DefType.Opt(inner1), DefType.Opt(inner2)) => inner1.merge(inner2) match {
          case d if d.isOpt => d
          case d => d.opt
        }
      case (_, DefType.Obj(_)) if thatType != thisType =>
        (that.defType match {
          case DefType.Obj(_) => that.merge(Definition(thisType))
          case _ => that
        }) match {
          case d if d.isOpt => d
          case d => d.opt
        }
      case _ if this.opt.defType == thatType => that
      case _ if thisType == that.opt.defType => this
      case _ if this.isOpt || that.isOpt => Definition(DefType.Opt(Definition(DefType.Str)))
      case _ if thisType == DefType.Str || thatType == DefType.Str => Definition(DefType.Str)
      case _ => throw new RuntimeException(s"Incompatible types:\n$this\n\n$that")
    }
  }
}

object Definition {

  /**
    * Binary-compatibility shim for callers compiled against the pre-constraints
    * 8-argument `Definition.apply`. Forwards to the full constructor with
    * `constraints = Constraints.Empty`. Can be removed when all downstream
    * consumers have been recompiled against this version.
    */
  def apply(
    defType: DefType,
    className: Option[String],
    description: Option[String],
    genericTypes: List[GenericType],
    genericName: Option[String],
    format: Format,
    defaultValue: Option[Json],
    deprecated: Boolean
  ): Definition = new Definition(defType, className, description, genericTypes, genericName, format, defaultValue, deprecated, Constraints.Empty)

  /**
    * Annotates fields of an Obj-typed Definition with format values from `@format` annotations.
    */
  def applyFieldFormats(d: Definition, formats: Map[String, Format]): Definition =
    if (formats.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
          formats.get(k).fold(k -> v)(fmt => k -> v.copy(format = fmt))
        }))
      case _ => d
    }

  /**
    * Marks fields as deprecated based on `@fieldDeprecated` annotations.
    */
  def applyFieldDeprecations(d: Definition, fields: Set[String]): Definition =
    if (fields.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
          if (fields.contains(k)) k -> v.copy(deprecated = true) else k -> v
        }))
      case _ => d
    }

  /**
    * Applies default values to fields. Default values are captured at compile time from case class defaults.
    */
  def applyFieldDefaults(d: Definition, defaults: Map[String, Json]): Definition =
    if (defaults.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
          defaults.get(k).fold(k -> v)(dv => k -> v.copy(defaultValue = Some(dv)))
        }))
      case _ => d
    }

  /**
    * Annotates fields of an Obj-typed Definition with validation constraints from per-field annotations
    * (`@pattern`, `@minLength`, `@maximum`, etc.).
    */
  def applyFieldConstraints(d: Definition, constraints: Map[String, Constraints]): Definition =
    if (constraints.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
          constraints.get(k).fold(k -> v)(c => k -> v.copy(constraints = c))
        }))
      case _ => d
    }

  /**
    * Annotates fields of an Obj-typed Definition with their generic type parameter names. Used by macro-generated RW
    * instances to connect fields like `value: T` back to the type parameter `"T"`.
    */
  def applyGenericNames(d: Definition, fieldGenericNames: Map[String, String]): Definition =
    if (fieldGenericNames.isEmpty) d
    else d.defType match {
      case o: DefType.Obj => d.copy(defType = o.copy(map = o.map.map { case (k, v) =>
          fieldGenericNames.get(k).fold(k -> v)(gn => k -> v.copy(genericName = Some(gn)))
        }))
      case _ => d
    }

  implicit def rw: RW[Definition] = RW.from[Definition](r = toJson, w = fromJson, d = Definition(DefType.Json))

  private def withOptional(base: fabric.Obj, key: String, value: Option[String]): fabric.Obj = value match {
    case Some(v) => base.merge(fabric.Obj(key -> str(v))).asObj
    case None => base
  }

  private def toJson(d: Definition): Json = {
    val base = d.defType match {
      case DefType.Obj(map) => obj(
          "type" -> str("object"),
          "values" -> fabric.Obj(map.map { case (key, inner) => key -> toJson(inner) })
        )
      case DefType.Arr(t) => obj("type" -> str("array"), "value" -> toJson(t))
      case DefType.Opt(t) => obj("type" -> str("optional"), "value" -> toJson(t))
      case DefType.Str => obj("type" -> str("string"))
      case DefType.Int => obj("type" -> str("numeric"), "precision" -> str("integer"))
      case DefType.Dec => obj("type" -> str("numeric"), "precision" -> str("decimal"))
      case DefType.Bool => obj("type" -> str("boolean"))
      case DefType.Json => obj("type" -> str("json"))
      case DefType.Null => obj("type" -> str("null"))
      case DefType.Poly(values) => obj(
          "type" -> str("poly"),
          "values" -> fabric.Obj(values.map { case (key, inner) => key -> toJson(inner) })
        )
    }
    var result = withOptional(
      withOptional(withOptional(base, "className", d.className), "description", d.description),
      "genericName",
      d.genericName
    )
    if (d.genericTypes.nonEmpty) result = result
      .merge(
        fabric.Obj(
          "genericTypes" -> Arr(
            d.genericTypes.map(gt => obj("name" -> str(gt.name), "definition" -> toJson(gt.definition))).toVector
          )
        )
      )
      .asObj
    if (d.format != Format.Raw) result = result.merge(fabric.Obj("format" -> str(d.format.name))).asObj
    d.defaultValue.foreach { dv =>
      result = result.merge(fabric.Obj("default" -> dv)).asObj
    }
    if (d.deprecated) result = result.merge(fabric.Obj("deprecated" -> bool(true))).asObj
    if (d.constraints.nonEmpty) {
      val c = d.constraints
      val pairs: List[(String, Json)] = List(
        c.pattern.map("pattern" -> str(_)),
        c.minLength.map("minLength" -> num(_)),
        c.maxLength.map("maxLength" -> num(_)),
        c.minimum.map("minimum" -> num(_)),
        c.maximum.map("maximum" -> num(_)),
        c.exclusiveMinimum.map("exclusiveMinimum" -> num(_)),
        c.exclusiveMaximum.map("exclusiveMaximum" -> num(_)),
        c.multipleOf.map("multipleOf" -> num(_)),
        c.minItems.map("minItems" -> num(_)),
        c.maxItems.map("maxItems" -> num(_)),
        c.uniqueItems.map("uniqueItems" -> bool(_))
      ).flatten
      if (pairs.nonEmpty) result = result.merge(fabric.Obj(pairs*)).asObj
    }
    result
  }

  private def fromJson(v: Json): Definition = {
    val o = v.asObj
    val cn = o.get("className").map(_.asString)
    val desc = o.get("description").map(_.asString)
    val dt = o.value("type").asString match {
      case "object" => DefType.Obj(o.value("values").asMap.map { case (key, value) => key -> fromJson(value) })
      case "array" => DefType.Arr(fromJson(o.value("value")))
      case "optional" => DefType.Opt(fromJson(o.value("value")))
      case "string" => DefType.Str
      case "numeric" => o.value("precision").asString match {
          case "integer" => DefType.Int
          case "decimal" => DefType.Dec
        }
      case "boolean" => DefType.Bool
      case "json" => DefType.Json
      case "null" => DefType.Null
      case "enum" =>
        val values = o.value("values").asVector.toList
        DefType.Poly(VectorMap(values.map(v => v.asString -> Definition(DefType.Null))*))

      case "poly" => DefType.Poly(o.value("values").asMap.map { case (key, json) => key -> fromJson(json) })
    }
    val gn = o.get("genericName").map(_.asString)
    val gt = o
      .get("genericTypes")
      .map(_.asVector.toList.map { gtJson =>
        val gtObj = gtJson.asObj
        GenericType(gtObj.value("name").asString, fromJson(gtObj.value("definition")))
      })
      .getOrElse(Nil)
    val fmt =
      o.get("format").map(f => Format.values.find(_.name == f.asString).getOrElse(Format.Raw)).getOrElse(Format.Raw)
    val dv = o.get("default")
    val dep = o.get("deprecated").exists(_.asBool.value)
    val cs = Constraints(
      pattern = o.get("pattern").map(_.asString),
      minLength = o.get("minLength").map(_.asInt),
      maxLength = o.get("maxLength").map(_.asInt),
      minimum = o.get("minimum").map(_.asDouble),
      maximum = o.get("maximum").map(_.asDouble),
      exclusiveMinimum = o.get("exclusiveMinimum").map(_.asDouble),
      exclusiveMaximum = o.get("exclusiveMaximum").map(_.asDouble),
      multipleOf = o.get("multipleOf").map(_.asDouble),
      minItems = o.get("minItems").map(_.asInt),
      maxItems = o.get("maxItems").map(_.asInt),
      uniqueItems = o.get("uniqueItems").map(_.asBool.value)
    )
    Definition(dt, cn, desc, gt, gn, fmt, dv, dep, cs)
  }
}

/**
  * GenericType represents a resolved type parameter of a generic class.
  *
  * For example, given `case class Wrapper[T](value: T)` and `RW.gen[Wrapper[String]]`, the resulting Definition's
  * `genericTypes` contains `GenericType("T", Definition(DefType.Str))` — recording that the type parameter named `"T"`
  * was resolved to `String`.
  *
  * @param name
  *   the type parameter name as declared in the source code (e.g. `"T"`, `"A"`, `"K"`)
  * @param definition
  *   the resolved Definition for this type parameter
  *
  * @see [[Definition.genericTypes]] for where these are stored
  * @see [[Definition.genericName]] for the field-level back-reference to a type parameter
  */
case class GenericType(name: String, definition: Definition)

object GenericType {
  implicit lazy val rw: RW[GenericType] = RW.from[GenericType](
    r = gt => obj("name" -> str(gt.name), "definition" -> gt.definition.json),
    w = j => GenericType(j("name").asString, j("definition").as[Definition]),
    d = Definition(
      DefType.Obj("name" -> Definition(DefType.Str), "definition" -> Definition(DefType.Json)),
      className = Some("fabric.define.GenericType")
    )
  )
}
