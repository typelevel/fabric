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

import fabric.rw.RW

/**
  * Format describes the semantic format of a value, typically used for string types in schema generation (e.g. OpenAPI).
  *
  * `Raw` indicates no specific format — the value is used as-is. All other variants correspond to well-known formats
  * from the OpenAPI specification or JSON Schema.
  *
  * Format is stored on [[Definition]] and can be set via the `@format` annotation on case class fields:
  * {{{
  * case class User(
  *   @format(Format.Email) email: String,
  *   @format(Format.DateTime) createdAt: String
  * )
  * }}}
  */
sealed trait Format {
  def name: String
}

object Format {
  implicit lazy val rw: RW[Format] = RW.enumeration[Format](values, _.name, caseSensitive = true)

  val values: List[Format] = List(Raw, Date, DateTime, Time, Email, Uri, Uuid, Hostname, Ipv4, Ipv6, Binary, Password)

  /**
    * No specific format — the value is used as-is.
    */
  case object Raw extends Format { val name = "raw" }

  /**
    * A date string (e.g. `"2024-01-15"`). OpenAPI format: `date`.
    */
  case object Date extends Format { val name = "date" }

  /**
    * A date-time string (e.g. `"2024-01-15T10:30:00Z"`). OpenAPI format: `date-time`.
    */
  case object DateTime extends Format { val name = "date-time" }

  /**
    * A time string (e.g. `"10:30:00"`).
    */
  case object Time extends Format { val name = "time" }

  /**
    * An email address. OpenAPI format: `email`.
    */
  case object Email extends Format { val name = "email" }

  /**
    * A URI/URL. OpenAPI format: `uri`.
    */
  case object Uri extends Format { val name = "uri" }

  /**
    * A UUID string (e.g. `"550e8400-e29b-41d4-a716-446655440000"`). OpenAPI format: `uuid`.
    */
  case object Uuid extends Format { val name = "uuid" }

  /**
    * A hostname. OpenAPI format: `hostname`.
    */
  case object Hostname extends Format { val name = "hostname" }

  /**
    * An IPv4 address. OpenAPI format: `ipv4`.
    */
  case object Ipv4 extends Format { val name = "ipv4" }

  /**
    * An IPv6 address. OpenAPI format: `ipv6`.
    */
  case object Ipv6 extends Format { val name = "ipv6" }

  /**
    * Binary data (e.g. base64-encoded). OpenAPI format: `binary`.
    */
  case object Binary extends Format { val name = "binary" }

  /**
    * A password/secret value (hints to UI to mask the field). OpenAPI format: `password`.
    */
  case object Password extends Format { val name = "password" }
}
