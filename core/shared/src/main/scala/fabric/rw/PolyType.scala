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

import fabric.Json
import fabric.define.{Definition, DefType}

import scala.reflect.ClassTag

/**
  * `PolyType[T]` is an open polymorphic registry — a thin wrapper around [[RW.poly]] that allows subtypes of `T` to be
  * registered dynamically at runtime, after the trait/object is defined.
  *
  * This is the typical design choice for ''open'' hierarchies: a library defines a trait `T` with some baseline
  * implementations, and downstream applications add their own subtypes without modifying the original. The standard
  * [[RW.poly]] (and `RW.gen` for sealed traits / unions) requires all subtypes to be known at compile time at the
  * call site — `PolyType` lifts that restriction.
  *
  * '''⚠️ Mutability warning:''' `PolyType` is the one place in `fabric` that uses mutable state — its registry of
  * subtypes is a `var` updated by [[register]]. This is an intentional trade-off to support open-hierarchy use
  * cases, but it has implications:
  *
  *   - '''Register at startup, before any serialization.''' Anyone who reads a `Definition` or serializes a value
  *     before registration sees an incomplete poly. Lazy `Definition` accesses elsewhere in your object graph can
  *     snapshot an empty `Poly` — often visible as missing dispatchers in generated schemas/code.
  *   - '''Single source of registrations.''' Centralize registrations in one place (e.g. a `Registrations` object
  *     called from your application's main). Scattered registrations make ordering bugs hard to reproduce.
  *   - '''Class-loader / module ordering.''' If multiple modules register subtypes, ensure the first call happens
  *     after all modules are loaded.
  *
  * If your hierarchy is ''closed'' (all subtypes known at compile time), prefer the immutable [[RW.poly]] or
  * `RW.gen` for sealed traits / unions instead.
  *
  * Example:
  * {{{
  *   trait Mode { def name: String }
  *   case object ConversationMode extends Mode { val name = "ConversationMode" }
  *   object Mode extends PolyType[Mode] {
  *     // Built-in subtype registered eagerly:
  *     register(RW.static(ConversationMode))
  *   }
  *
  *   // Apps add their own at startup:
  *   case object WorkflowMode extends Mode { val name = "WorkflowMode" }
  *   Mode.register(RW.static(WorkflowMode))
  * }}}
  *
  * @tparam T
  *   the polymorphic base type
  */
abstract class PolyType[T](implicit protected val classTag: ClassTag[T]) {

  @volatile private var types: List[RW[? <: T]] = Nil
  @volatile private var _poly: RW[T] = generate()

  /**
    * Build the underlying RW from the registered subtype list, then overlay the
    * type-compatible intersection of subtype field maps as `commonFields` on the
    * poly Definition. Codegen consumers (e.g. spice's Dart generator) read
    * `commonFields` directly to emit abstract-parent fields without re-deriving
    * the intersection — and without needing to know about the parent trait's
    * surface separately from each subtype's.
    */
  private def generate(): RW[T] = {
    val baseRw = RW.poly[T]()(types*)
    new RW[T] {
      override def read(t: T): Json = baseRw.read(t)
      override def write(json: Json): T = baseRw.write(json)
      override def definition: Definition = withCommonFields(baseRw.definition)
    }
  }

  private def withCommonFields(d: Definition): Definition = d.defType match {
    case p: DefType.Poly => d.copy(defType = p.copy(commonFields = computeCommonFields(p)))
    case _ => d
  }

  /**
    * Intersect the field maps of every subtype's `DefType.Obj`, keeping
    * entries whose names + types match across ALL subtypes. Subtypes whose
    * Definition isn't an Obj (case-object-style enum cases backed by
    * `DefType.Null`, or other non-record shapes) contribute no fields and
    * therefore cause the intersection to drop to empty — that's the right
    * answer for a heterogeneous poly. Empty when there are no registered
    * subtypes.
    */
  private def computeCommonFields(p: DefType.Poly): Map[String, Definition] =
    if (p.values.isEmpty) Map.empty
    else {
      val perSubtype: List[Map[String, Definition]] = p.values.values.toList.map { defn =>
        defn.defType match {
          case o: DefType.Obj => o.map.toMap
          case _ => Map.empty[String, Definition]
        }
      }
      if (perSubtype.exists(_.isEmpty)) Map.empty
      else {
        val commonNames = perSubtype.iterator.map(_.keySet).reduce(_ intersect _)
        commonNames.iterator.flatMap { name =>
          val defs = perSubtype.flatMap(_.get(name))
          // Keep the field only if every subtype's entry agrees on the
          // serialized type. Compares `defType` shape — same primitive,
          // same Obj structure, etc. — to avoid promoting a name that
          // means different things on different subtypes.
          val head = defs.head
          if (defs.forall(_.defType == head.defType)) Some(name -> head) else None
        }.toMap
      }
    }

  /**
    * Register additional `T` subtypes into the poly RW.
    *
    * Call this at backend startup, before any serialization or `Definition` access. Any `Definition` snapshots taken
    * before registration will not see the newly registered subtypes.
    */
  def register(types: RW[? <: T]*): Unit = synchronized {
    this.types = this.types ++ types.toList
    _poly = generate()
  }

  private def shortName(fullName: String): String = {
    val lastDot = fullName.lastIndexOf('.')
    val lastDollar = fullName.lastIndexOf('$')
    val start = math.max(lastDot, lastDollar) + 1
    fullName.substring(start)
  }

  /**
    * Namespace for typed-name construction and lookup against this `PolyType`'s live registration.
    *
    *   - `name.of(instance)` — derive a `PolyName[T]` from a concrete instance
    *   - `name.of[S]` — derive a `PolyName[T]` from a subtype at compile time
    *   - `name.from(s)` — validated lookup against registered subtypes
    *   - `name.registered` — the live set of registered names
    *
    * Each `PolyType`'s `name.*` naturally scopes to its own registration — no need to pass the `PolyType` reference
    * around.
    */
  object name {

    /**
      * Build a `PolyName[T]` from a concrete `T` instance. Uses the simple class name (trailing `$` stripped for
      * case objects).
      */
    def of(instance: T): PolyName[T] = new PolyName[T](instance.getClass.getSimpleName.stripSuffix("$"))

    /**
      * Build a `PolyName[T]` from a subtype at compile time. Uses the simple class name (trailing `$` stripped for
      * case objects).
      */
    def of[S <: T](implicit ct: ClassTag[S]): PolyName[T] =
      new PolyName[T](ct.runtimeClass.getSimpleName.stripSuffix("$"))

    /**
      * Validated lookup: returns `Some(PolyName)` if `n` matches a registered subtype, else `None`.
      */
    def from(n: String): Option[PolyName[T]] = if (registered.exists(_.name == n)) Some(new PolyName[T](n)) else None

    /**
      * The set of subtype names currently registered. Derived from each registered RW's `Definition.className`.
      */
    def registered: Set[PolyName[T]] = synchronized {
      types.flatMap(_.definition.className.map(n => new PolyName[T](shortName(n)))).toSet
    }
  }

  /**
    * The polymorphic RW. Subclasses can expose this as their own `rw` if they need to combine `PolyType` with another
    * RW interface (avoiding self-recursion that summoning the given would cause).
    */
  protected val polyRW: RW[T] = new RW[T] {
    override def read(t: T): Json = _poly.read(t)
    override def write(json: Json): T = _poly.write(json)
    override def definition: Definition = _poly.definition
  }

  implicit val rw: RW[T] = polyRW
}

object PolyType {

  /**
    * Construct a `PolyType[T]` from a `ClassTag`. Useful when you want a standalone registry rather than mixing
    * `PolyType` into a companion object.
    *
    * Example:
    * {{{
    *   val Modes = PolyType[Mode]
    *   Modes.register(RW.static(ConversationMode))
    * }}}
    */
  def apply[T](implicit ct: ClassTag[T]): PolyType[T] = new PolyType[T] {}
}
