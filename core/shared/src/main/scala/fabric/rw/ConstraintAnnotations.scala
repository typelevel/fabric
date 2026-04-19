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

import scala.annotation.StaticAnnotation

/**
  * Constrain a string field's values to match the given regular expression. Mirrors JSON Schema / OpenAPI `pattern`.
  *
  * Example:
  * {{{
  * case class User(@pattern("^[a-z]+@[a-z]+\\..+$") email: String)
  * }}}
  */
class pattern(val value: String) extends StaticAnnotation

/** Minimum length of a string field. Mirrors JSON Schema `minLength`. */
class minLength(val value: Int) extends StaticAnnotation

/** Maximum length of a string field. Mirrors JSON Schema `maxLength`. */
class maxLength(val value: Int) extends StaticAnnotation

/** Minimum (inclusive) numeric value. Mirrors JSON Schema `minimum`. */
class minimum(val value: Double) extends StaticAnnotation

/** Maximum (inclusive) numeric value. Mirrors JSON Schema `maximum`. */
class maximum(val value: Double) extends StaticAnnotation

/** Minimum (exclusive) numeric value. Mirrors JSON Schema `exclusiveMinimum`. */
class exclusiveMinimum(val value: Double) extends StaticAnnotation

/** Maximum (exclusive) numeric value. Mirrors JSON Schema `exclusiveMaximum`. */
class exclusiveMaximum(val value: Double) extends StaticAnnotation

/** Numeric value must be a multiple of this. Mirrors JSON Schema `multipleOf`. */
class multipleOf(val value: Double) extends StaticAnnotation

/** Minimum number of array items. Mirrors JSON Schema `minItems`. */
class minItems(val value: Int) extends StaticAnnotation

/** Maximum number of array items. Mirrors JSON Schema `maxItems`. */
class maxItems(val value: Int) extends StaticAnnotation

/** All array items must be unique. Mirrors JSON Schema `uniqueItems`. */
class uniqueItems(val value: Boolean = true) extends StaticAnnotation
