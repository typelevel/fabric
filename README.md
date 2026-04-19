# fabric

[![CI](https://github.com/typelevel/fabric/actions/workflows/ci.yml/badge.svg)](https://github.com/typelevel/fabric/actions/workflows/ci.yml)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/typelevel/fabric)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.typelevel/fabric-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/fabric-core_2.13)
[![fabric-core Scala version support](https://index.scala-lang.org/typelevel/fabric/fabric-core/latest.svg)](https://index.scala-lang.org/typelevel/fabric/fabric-core)

Abstract Syntax Tree (AST) based on JSON concepts, but more abstract for parsing and application.

## Justification

Having worked with Circe and uPickle for years there are many things I love about each, but unfortunately a
few things I was frustrated by. At a high level, I think Circe can be a bit overly complicated and compilation
quite slow in large projects. With uPickle, I found the mutable underlying references within the structure very
concerning and problematic when doing things like merges. Both of them suffer from slow releases periodically,
so I ultimately decided to try my hand at accomplishing the same and incorporate some of my own crazy ideas in
the process.

I won't say that fabric is a better library than either of those great projects, but it was inspired by
both of them and customized to suit my particular needs. If you find it useful as well, please use it and offer
some feedback.

## Performance

I wrote a performance benchmark with every expectation to be slower than the alternatives as I've done very
little tuning, and I'm just one person versus the many developers that have worked on the others for years.
However, I was shocked to see how well my little library performed compared to the alternatives:
[JMH Results for 1.7.0 on Scala 3](https://jmh.morethan.io/?source=https://raw.githubusercontent.com/typelevel/fabric/master/bench/results/benchmarks-1.7.0.json)

## Features

The focus of this project is minimalism and flexibility. To that end, the features are somewhat sparse:

- Support for JVM, Scala.js, and Scala Native
- Support for Scala 2.12, 2.13, and 3.x
- AST for representation of `Map`, `Array`, `Numeric`, `String`, `Boolean`, and `null` in a type-safe and immutable way
- Clean DSL to create tree structures
- Deep merging support
- Compile-time generation of conversions to/from case classes with support for default arguments
- Rich schema metadata on `Definition` — class names, descriptions, formats, deprecation flags, defaults, generic type parameters, and value-level constraints
- Annotation-driven schema enrichment (`@description`, `@format`, `@fieldDeprecated`, `@pattern`, `@minLength`, `@maximum`, etc.)
- Generic type tracking — distinguish erased generic variants via the `_generic` discriminator
- Union type support (Scala 3), including collision handling for types like `Id[String] | Id[Int]`
- Easy and convenient extensibility support
- Parsing support for JSON on JVM and Scala.js
- JSON DDL generation from data
- Scala code generation from JSON DDL (data → DDL → Scala) for easy data mapping

## Getting Started

### Setup

For SBT simply include:
`libraryDependencies += "org.typelevel" %%% "fabric-core" % "1.25.0"`

For parsing support include:
`libraryDependencies += "org.typelevel" %%% "fabric-io" % "1.25.0"`

### Create

Creating fabric structures with the DSL is very easy:

```scala
import fabric._
import fabric.dsl._

val v1 = obj(
  "name" -> "John Doe",
  "age" -> 21,
  "numbers" -> List(1, 2, 3),
  "address" -> obj(
    "street" -> "123 Somewhere Rd.",
    "city" -> "San Jose"
  )
)
// v1: Obj = {"name": "John Doe", "age": 21, "numbers": [1, 2, 3], "address": {"street": "123 Somewhere Rd.", "city": "San Jose"}}
```

### Merging

Deep-merging is trivial:

```scala
import fabric._

val v2 = obj(
  "age" -> 23,
  "numbers" -> List(4, 5, 6),
  "address" -> obj(
    "state" -> "California"
  )
)
// v2: Obj = {"age": 23, "numbers": [4, 5, 6], "address": {"state": "California"}}

val v3 = v1.merge(v2)
// v3: Json = {"name": "John Doe", "age": 23, "numbers": [4, 5, 6], "address": {"street": "123 Somewhere Rd.", "city": "San Jose", "state": "California"}}
```

It is worth mentioning that because values are immutable, `v1` and `v2` remain unchanged.

### Convert

Conversion to other types is very easy with the built-in compile-time conversions:

```scala
import fabric._
import fabric.rw._

val person = obj(
  "name" -> "John Doe",
  "age" -> 21
).as[Person]
// person: Person = Person(name = "John Doe", age = 21)

val backToValue: Json = person.json
// backToValue: Json = {"name": "John Doe", "age": 21}

case class Person(name: String, age: Int)

object Person {
  implicit val rw: RW[Person] = RW.gen[Person]
}
```

### Parse

Parsing from existing JSON:

```scala
import fabric.io._

val value = JsonParser("""{"name": "John Doe", "age": 21}""", Format.Json)
// value: Json = {"name": "John Doe", "age": 21}
```

### Formatting

Taking an existing value and formatting it for output as JSON:

```scala
val formattedString: String = JsonFormatter.Default(value)
// formattedString: String = """{
//   "name": "John Doe",
//   "age": 21
// }"""
```

## Schema Definitions

Every `RW[T]` exposes a `definition: Definition` that describes the structural type and carries rich schema metadata.
This is the foundation for schema generation, validation, OpenAPI output, and code generation.

### DefType vs Definition

Two types work together:

- **`DefType`** — the pure JSON type structure. Variants: `Obj`, `Arr`, `Opt`, `Str`, `Int`, `Dec`, `Bool`, `Json`,
  `Null`, `Poly`. No metadata — just "what kind of value is this?"
- **`Definition`** — wraps a `DefType` with metadata: `className`, `description`, `format`, `defaultValue`,
  `deprecated`, `genericTypes`, `genericName`, and `constraints`.

`RW[T].definition` returns a `Definition`, not a raw `DefType`.

### Annotations

Annotate case class fields to enrich the generated `Definition`:

```scala
import fabric.rw._
import fabric.define.Format

case class User(
  @description("The user's full name") name: String,
  @format(Format.Email) email: String,
  @format(Format.DateTime) createdAt: String,
  @fieldDeprecated legacyId: Option[String] = None,
  age: Int = 0
)

object User {
  implicit val rw: RW[User] = RW.gen[User]
}
```

| Annotation         | Purpose                                                              |
|--------------------|----------------------------------------------------------------------|
| `@description`     | Human-readable description for the field                             |
| `@format`          | Semantic string format: `Email`, `Uri`, `DateTime`, `Uuid`, etc.     |
| `@fieldDeprecated` | Marks a field as deprecated                                          |
| `@serialized`      | Include a `val`/`def` member in the JSON output                      |
| `@notSerialized`   | Exclude a constructor param from the JSON output                     |
| `@typeField`       | Customize the type-discriminator field name for sealed traits/unions |

Default values from case class parameters are captured automatically into `Definition.defaultValue`.

### Constraints

Value-level validation constraints — mirroring JSON Schema / OpenAPI — are expressed via annotations and stored on
`Definition.constraints`:

```scala
import fabric.rw._

case class Account(
  @pattern("^[a-z]+@[a-z]+\\..+$") @maxLength(254) email: String,
  @minimum(0) @maximum(150) age: Int,
  @minItems(1) @uniqueItems roles: List[String]
)

object Account {
  implicit val rw: RW[Account] = RW.gen[Account]
}
```

Available constraint annotations:

| Annotation          | Applies to | Purpose                                |
|---------------------|------------|----------------------------------------|
| `@pattern`          | strings    | Regex the value must match             |
| `@minLength`        | strings    | Minimum length                         |
| `@maxLength`        | strings    | Maximum length                         |
| `@minimum`          | numerics   | Minimum value (inclusive)              |
| `@maximum`          | numerics   | Maximum value (inclusive)              |
| `@exclusiveMinimum` | numerics   | Minimum value (exclusive)              |
| `@exclusiveMaximum` | numerics   | Maximum value (exclusive)              |
| `@multipleOf`       | numerics   | Must be a multiple of this value       |
| `@minItems`         | arrays     | Minimum number of items                |
| `@maxItems`         | arrays     | Maximum number of items                |
| `@uniqueItems`      | arrays     | All items must be unique               |

### Generic Type Tracking

When `RW.gen` generates an RW for a generic case class, the resulting `Definition` records which type parameters were
resolved and which fields reference them:

```scala
import fabric.rw._
import fabric.define._

case class Wrapper[T](name: String, value: T)

object Wrapper {
  implicit def rw[T: RW]: RW[Wrapper[T]] = RW.gen[Wrapper[T]]
}

val d = implicitly[RW[Wrapper[String]]].definition
// d.genericTypes == List(GenericType("T", Definition(DefType.Str)))
// d.defType.asInstanceOf[DefType.Obj].map("value").genericName == Some("T")
```

Additionally, the serialized JSON for a generic case class includes a `_generic` field:

```json
{"name": "Test", "value": "hello", "_generic": {"T": {"type": "string"}}}
```

This enables disambiguating erased generic variants at deserialization time. Set `RW.SerializeGenerics = false` to
suppress this field globally (e.g. for cleaner output when you don't need the disambiguation).

### Union Types (Scala 3)

Scala 3 union types are supported via `RW.gen`:

```scala
case class Cat(name: String, age: Int) derives RW
case class Dog(name: String, breed: String) derives RW

given RW[Cat | Dog] = RW.gen[Cat | Dog]
```

For collision unions where multiple variants share the same base class (e.g. `Id[String] | Id[Int]`), the `_generic`
field is used to distinguish between them on the deserialization side.

### Inspection & Generation

```scala
// Validate a JSON value against a Definition
definition.validate(json)

// Generate a template JSON value from a Definition
definition.template(TemplateConfig.Empty)

// Infer a Definition from raw JSON data
val inferred: Definition = FabricDefinition(json)

// Generate Scala case class source from a Definition
val generated = FabricGenerator(definition, "com.example.Person", _ => "com.example.Nested")
```

### Serialization of Definitions

`Definition` has its own `RW[Definition]` for JSON round-tripping of schema metadata — useful for persisting
schemas, sending them over the wire, or rendering them as OpenAPI. All fields — `className`, `description`,
`format`, `defaultValue`, `deprecated`, `genericTypes`, `genericName`, `constraints` — are preserved.

## Validation

The `validate.sh` script runs the same checks as CI (headers, formatting, fatal-warning compile, full test suite
across all Scala versions and platforms). Run it before pushing to catch issues locally.
