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

import scala.collection.immutable.VectorMap

/**
  * DefType represents the pure structural JSON type of a value, without any metadata such as class names, descriptions,
  * or generic type information. It is the inner "shape" of a [[Definition]].
  *
  * DefType is a sealed trait with variants for each JSON-compatible type:
  *   - '''Obj''' — an object/record with named fields, each having its own [[Definition]]
  *   - '''Arr''' — an array/list of a single element [[Definition]]
  *   - '''Opt''' — an optional value wrapping a [[Definition]] (maps to `scala.Option`)
  *   - '''Str''' — a string value
  *   - '''Int''' — an integer numeric value
  *   - '''Dec''' — a decimal numeric value
  *   - '''Bool''' — a boolean value
  *   - '''Json''' — an opaque/arbitrary JSON value
  *   - '''Null''' — a null/absent value
  *   - '''Poly''' — a polymorphic type (sealed trait, enum, or union) with named variants, each having its own
  *     [[Definition]]. Simple string enums use `Poly` with `Definition(DefType.Null)` for each case.
  *
  * DefType intentionally carries no metadata. All metadata (class names, descriptions, generic type parameters, and
  * field-to-generic mappings) lives on the wrapping [[Definition]]. This separation keeps the type structure clean and
  * composable, while [[Definition]] provides the full picture needed for code generation, schema output (e.g. OpenAPI),
  * and serialization.
  *
  * @see [[Definition]] for the metadata-carrying wrapper
  * @see [[GenericType]] for type parameter information
  */
sealed trait DefType {

  /**
    * Returns true if this is an optional type (`Opt`).
    */
  def isOpt: Boolean = false

  /**
    * Returns true if this is a null type (`Null`).
    */
  def isNull: Boolean = false
}

object DefType {

  /**
    * An object/record type with named fields. Each field maps to a [[Definition]] which carries its own type, metadata,
    * and optional `genericName` indicating which type parameter the field references.
    */
  case class Obj(map: Map[String, Definition]) extends DefType
  object Obj {
    def apply(entries: (String, Definition)*): Obj = Obj(VectorMap(entries*))
  }

  /**
    * An array/list type. The element type is a [[Definition]].
    */
  case class Arr(t: Definition) extends DefType

  /**
    * An optional type wrapping an inner [[Definition]]. Maps to `scala.Option`.
    */
  case class Opt(t: Definition) extends DefType {
    override def isOpt: Boolean = true
  }

  /**
    * A string value type.
    */
  case object Str extends DefType

  /**
    * An integer numeric value type.
    */
  case object Int extends DefType

  /**
    * A decimal numeric value type.
    */
  case object Dec extends DefType

  /**
    * A boolean value type.
    */
  case object Bool extends DefType

  /**
    * An opaque/arbitrary JSON value type.
    */
  case object Json extends DefType

  /**
    * A null/absent value type.
    */
  case object Null extends DefType {
    override def isNull: Boolean = true
  }

  /**
    * A polymorphic type representing sealed traits, enums, or union types. Each variant maps a name to its
    * [[Definition]].
    *
    * For simple string enums (all case objects), each variant has `Definition(DefType.Null)` since the cases carry no
    * data — only the variant name matters. For sealed traits or parameterized enums, each variant has a full
    * `Definition(DefType.Obj(...))` describing its fields.
    *
    * Examples:
    *   - Simple enum `Color { Red, Green, Blue }` →
    *     `Poly(Map("Red" -> Definition(Null), "Green" -> Definition(Null), "Blue" -> Definition(Null)))`
    *   - Sealed trait `Shape { Circle(r: Double), Rect(w: Double, h: Double) }` →
    *     `Poly(Map("Circle" -> Definition(Obj("r" -> ...)), "Rect" -> Definition(Obj("w" -> ..., "h" -> ...))))`
    */
  /**
    * Polymorphic Definition.
    *
    * @param values        per-subtype Definition map (key is the subtype's discriminator on the wire — typically
    *                      `Product.productPrefix` for case classes, or the enum case name).
    * @param commonFields  fields shared by every subtype in `values`. Computed by [[fabric.rw.PolyType.register]] as
    *                      the type-compatible intersection across each subtype's `DefType.Obj` field map. Empty for
    *                      compile-time `RW.poly` Definitions; populated for open registries built via
    *                      `PolyType[T]`. Codegen consumers use this to emit abstract-parent fields without
    *                      re-deriving the intersection.
    */
  case class Poly(values: Map[String, Definition], commonFields: Map[String, Definition] = Map.empty) extends DefType
  object Poly {
    def apply(entries: (String, Definition)*): Poly = Poly(VectorMap(entries*))
  }
}
