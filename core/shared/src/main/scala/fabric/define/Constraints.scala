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

/**
  * Constraints express value-level validation vocabulary that mirrors JSON Schema / OpenAPI. Stored on [[Definition]]
  * and populated from field annotations (`@pattern`, `@minLength`, `@maximum`, etc.) during RW derivation.
  *
  * Downstream consumers — JSON Schema emitters, OpenAPI generators, request validators — read these constraints to
  * enforce or document value-level rules that the structural `DefType` alone cannot express.
  *
  * Only attributes relevant to the underlying `DefType` have meaning (e.g. `pattern`/`minLength`/`maxLength` on strings,
  * `minimum`/`maximum`/`multipleOf` on numerics, `minItems`/`maxItems`/`uniqueItems` on arrays). Consumers should
  * silently ignore attributes that don't apply to a given type.
  *
  * @param pattern regex the string value must match (strings only)
  * @param minLength minimum string length (strings only)
  * @param maxLength maximum string length (strings only)
  * @param minimum minimum numeric value, inclusive (numerics only)
  * @param maximum maximum numeric value, inclusive (numerics only)
  * @param exclusiveMinimum exclusive minimum numeric value (numerics only)
  * @param exclusiveMaximum exclusive maximum numeric value (numerics only)
  * @param multipleOf numeric value must be a multiple of this (numerics only)
  * @param minItems minimum number of array items (arrays only)
  * @param maxItems maximum number of array items (arrays only)
  * @param uniqueItems whether all array items must be unique (arrays only)
  */
case class Constraints(
  pattern: Option[String] = None,
  minLength: Option[Int] = None,
  maxLength: Option[Int] = None,
  minimum: Option[Double] = None,
  maximum: Option[Double] = None,
  exclusiveMinimum: Option[Double] = None,
  exclusiveMaximum: Option[Double] = None,
  multipleOf: Option[Double] = None,
  minItems: Option[Int] = None,
  maxItems: Option[Int] = None,
  uniqueItems: Option[Boolean] = None
) {

  def isEmpty: Boolean = this == Constraints.Empty

  def nonEmpty: Boolean = !isEmpty
}

object Constraints {

  /** The absence of any constraints. */
  val Empty: Constraints = Constraints()
}
