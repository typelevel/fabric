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
import fabric.define.{Definition, DefType}

import scala.collection.immutable.VectorMap
import scala.reflect.ClassTag

/**
  * RW provides a single class representation of a Reader and Writer for the
  * same type
  */
trait RW[T] extends Reader[T] with Writer[T] {
  def definition: Definition

  def withPreWrite(f: Json => Json): RW[T] = EnhancedRW[T](this, preWrite = List(f))
  def withPostRead(f: (T, Json) => Json): RW[T] = EnhancedRW[T](this, postRead = List(f))
}

object RW extends CompileRW {

  /**
    * Controls whether generic type information (`_generic` field) is included in serialized JSON output for generic
    * case classes. Defaults to `true`. Set to `false` to exclude `_generic` from output, which produces cleaner JSON
    * but loses the ability to disambiguate erased generic variants during deserialization (e.g. in union types like
    * `Id[String] | Id[Int]`).
    */
  var SerializeGenerics: Boolean = true

  def from[T](r: T => Json, w: Json => T, d: => Definition): RW[T] = new RW[T] {
    override def write(value: Json): T = w(value)

    override def read(t: T): Json = r(t)

    override def definition: Definition = d
  }

  def enumeration[T: ClassTag](
    list: List[T],
    asString: T => String = (t: T) => Definition.simpleClassName(cleanClassName(t.getClass.getName)),
    caseSensitive: Boolean = false
  ): RW[T] = new RW[T] {
    val className: String = cleanClassName(implicitly[ClassTag[T]].runtimeClass.getName)

    private def fixString(s: String): String = if (caseSensitive) s else s.toLowerCase

    private lazy val map = list.map(t => fixString(asString(t)) -> t).toMap

    override def write(value: Json): T = map(fixString(value.asString))

    override def read(t: T): Json = str(asString(t))

    override val definition: Definition = Definition(
      DefType.Poly(VectorMap(list.map(t => asString(t) -> Definition(DefType.Null))*)),
      className = Some(className)
    )
  }

  def string[T](asString: T => String, fromString: String => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromString(value.asString)

    override def read(t: T): Json = str(asString(t))

    override val definition: Definition = Definition(DefType.Str, className = className)
  }

  def int[T](asInt: T => Int, fromInt: Int => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromInt(value.asInt)

    override def read(t: T): Json = asInt(t).json

    override val definition: Definition = Definition(DefType.Int, className = className)
  }

  def long[T](asLong: T => Long, fromLong: Long => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromLong(value.asLong)

    override def read(t: T): Json = asLong(t).json

    override val definition: Definition = Definition(DefType.Int, className = className)
  }

  def double[T](asDouble: T => Double, fromDouble: Double => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromDouble(value.asDouble)

    override def read(t: T): Json = num(asDouble(t))

    override val definition: Definition = Definition(DefType.Dec, className = className)
  }

  def dec[T](asDec: T => BigDecimal, fromDec: BigDecimal => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromDec(value.asBigDecimal)

    override def read(t: T): Json = num(asDec(t))

    override val definition: Definition = Definition(DefType.Dec, className = className)
  }

  def bool[T](asBool: T => Boolean, fromBool: Boolean => T, className: Option[String] = None): RW[T] = new RW[T] {
    override def write(value: Json): T = fromBool(value.asBool.value)

    override def read(t: T): Json = fabric.Bool(asBool(t))

    override val definition: Definition = Definition(DefType.Bool, className = className)
  }

  def wrapped[T](
    key: String,
    asJson: T => Json,
    fromJson: Json => T,
    definition: Definition = Definition(DefType.Json)
  ): RW[T] = RW.from(
    r = t => obj(key -> asJson(t)),
    w = j => fromJson(j(key)),
    d = Definition(DefType.Obj(key -> definition), className = Some(key))
  )

  /**
    * Convenience functionality to provide a static / singleton value that
    * represents that type
    *
    * @param value
    *   the singleton value to use
    */
  def static[T](value: T): RW[T] = from(
    _ => obj(),
    _ => value,
    Definition(DefType.Obj(VectorMap.empty[String, Definition]), className = Some(cleanClassName(value.getClass.getName)))
  )

  /**
    * Convenience functionality for working with polymorphic types.
    *
    * Dispatch keys are the class-chain form of each subtype's [[Definition.className]] — packages stripped,
    * nested-class chain preserved — so distinct enums declaring same-named cases (`Foo.Success` vs
    * `Bar.Success`) never collide. The wire JSON `"type"` field stamps the same string. Both registration
    * and instance dispatch derive the key via [[Definition.simpleClassName]], so the two sides are
    * symmetric by construction.
    *
    * A legacy-leaf fallback index lets persisted records written under the historical leaf-name
    * convention (`{"type": "Success"}`) continue to be read, provided the leaf is unambiguous across
    * registered subtypes. Ambiguous legacy discriminators throw at read time with an actionable message
    * (declare `typeAliases` to pin the intended target). Registration-time collision detection on the
    * primary keys fails fast if two registered RWs produce the same `simpleClassName`.
    *
    * @param fieldName
    *   the JSON field name carrying the type discriminator (defaults to "type")
    * @param typeAliases
    *   `("alias" -> "primaryKey")` entries that route an old wire discriminator to a registered subtype.
    *   Use this for renames, migrations off the legacy-leaf format, or pinning ambiguous legacy data.
    */
  def poly[P: ClassTag](
    fieldName: String = "type",
    typeAliases: List[(String, String)] = Nil
  )(
    types: RW[? <: P]*
  ): RW[P] = {
    def typeName(rw: RW[? <: P]): String =
      rw.definition.simpleClassName.getOrElse(throw new RuntimeException(s"No className defined for $rw"))

    // Auto-recurse into nested polymorphic subtypes: when a registered subtype's defType is itself a
    // Poly (e.g. the registered RW is an enum or sealed trait), each of its variants gets a typeMap entry
    // pointing back to the parent's RW. Instance dispatch on a variant type then routes through the
    // parent's RW (which handles the actual variant dispatch internally) without requiring per-variant
    // registration. No-op for flat hierarchies (case-class subtypes have non-Poly defTypes).
    val directPairs = types.toList.flatMap { rw =>
      val parent = typeName(rw) -> rw
      val nested = rw.definition.defType match {
        case p: DefType.Poly =>
          p.values.toList.flatMap { case (_, variantDefn) =>
            variantDefn.simpleClassName.map(_ -> rw)
          }
        case _ => Nil
      }
      parent :: nested
    }

    // Registration-time collision guardrail: two subtypes (or auto-recursed variants) producing the same
    // primary key is a registration bug — fail loud at startup rather than silently routing wrong at
    // runtime.
    directPairs.groupBy(_._1).collectFirst { case (key, occurrences) if occurrences.size > 1 =>
      val cns = occurrences.flatMap(_._2.definition.className).distinct.mkString(", ")
      throw new RuntimeException(
        s"Duplicate polymorphic dispatch key '$key' from multiple registered types: [$cns]. " +
          "Each subtype must produce a unique simpleClassName (class-chain form). Rename one of the " +
          "colliding types, or use typeAliases to pin distinct wire discriminators."
      )
    }
    val directTypeMappings = directPairs.toMap

    // Legacy-leaf fallback: case-insensitive index keyed by the leaf segment of each registered subtype's
    // className. Unambiguous matches dispatch through this fallback so persisted records using the
    // pre-1.29 wire format (`{"type": "Success"}`) continue to read. Ambiguous matches throw with a
    // typeAliases pointer rather than silently picking a winner.
    val legacyLeafIndex: Map[String, List[RW[? <: P]]] =
      types.toList.groupBy { rw =>
        val cn = rw.definition.className.getOrElse("")
        val leaf = cn.split('.').filter(_.nonEmpty).lastOption.getOrElse(cn)
        leaf.toLowerCase
      }

    val aliasedTypeMappings = typeAliases.map { case (alias, primary) =>
      // Look up primary by exact match (new simpleClassName form) first; fall back to case-insensitive
      // leaf-only match (legacy form) so aliases written against pre-1.29 fabric continue to work
      // without modification when they're unambiguous.
      val rw = directTypeMappings.get(primary).orElse {
        legacyLeafIndex.get(primary.toLowerCase) match {
          case Some(single :: Nil) => Some(single)
          case _ => None
        }
      }.getOrElse(
        throw new RuntimeException(
          s"Unable to map alias '$alias' → '$primary': '$primary' not found in registered keys " +
            s"[${directTypeMappings.keys.mkString(", ")}]"
        )
      )
      alias -> rw
    }.toMap

    val className: String = cleanClassName(implicitly[ClassTag[P]].runtimeClass.getName)
    val typeMap = directTypeMappings ++ aliasedTypeMappings

    def resolve(rawType: String): Option[RW[? <: P]] =
      typeMap.get(rawType).orElse {
        legacyLeafIndex.get(rawType.toLowerCase) match {
          case Some(single :: Nil) => Some(single)
          case Some(many) if many.size > 1 =>
            throw new RuntimeException(
              s"Ambiguous legacy discriminator '$rawType' — matches ${many.size} registered types: " +
                s"[${many.flatMap(_.definition.className).mkString(", ")}]. " +
                s"Persisted records using leaf-only discriminators are ambiguous across these types; " +
                s"add typeAliases to pin '$rawType' to one of them, or migrate the stored records to " +
                s"the new wire form."
            )
          case _ => None
        }
      }.map(_.asInstanceOf[RW[? <: P]])

    from(
      r = (p: P) => {
        val `type` = Definition.simpleClassName(cleanClassName(p.getClass.getName))
        resolve(`type`) match {
          case Some(rw) => rw.asInstanceOf[RW[P]].read(p).merge(obj(fieldName -> str(`type`)))
          case None => throw new RuntimeException(
              s"Type not found [${`type`}] converting from value $p. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      w = (json: Json) => {
        val rawType = json(fieldName).asString
        resolve(rawType) match {
          case Some(rw) => rw.write(json)
          case None => throw new RuntimeException(
              s"Type not found [$rawType] converting from value $json. Available types are: [${typeMap.keySet.mkString(", ")}]"
            )
        }
      },
      d = Definition(
        DefType.Poly(
          VectorMap(types.map { rw =>
            val key = typeName(rw)
            key -> rw.definition
          }*)
        ),
        className = Some(className)
      )
    )
  }

  def cleanClassName(className: String): String = className.replace("$u0020", " ").replace("$", ".") match {
    case s if s.endsWith(".") => s.substring(0, s.length - 1)
    case s => s
  }
}
