# fabric

[![CI](https://github.com/outr/fabric/actions/workflows/ci.yml/badge.svg)](https://github.com/outr/fabric/actions/workflows/ci.yml)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/outr/fabric)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.outr/fabric-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.outr/fabric-core_2.13)
[![fabric-core Scala version support](https://index.scala-lang.org/outr/fabric/fabric-core/latest-by-scala-version.svg)](https://index.scala-lang.org/outr/fabric/fabric-core)

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
[JMH Results for 1.4.0 on Scala 3](https://jmh.morethan.io/?source=https://raw.githubusercontent.com/outr/fabric/master/bench/results/benchmarks-1.4.0.json)
## Features

The focus of this project is minimalism and flexibility. To that end, the features are somewhat sparse:

- Support for JVM, Scala.js, and Scala Native
- Support for Scala 2.11, 2.12, 2.13, and 3.x
- AST for representation of `Map`, `Array`, `Numeric`, `String`, `Boolean`, and `null` in a type-safe and immutable way
- Clean DSL to create tree structures
- Deep merging support
- Compile-time generation of conversions to/from case classes with support for default arguments
- Easy and convenient extensibility support
- Parsing support for JSON on JVM and Scala.js
- JSON DDL generation from data
- Scala code generation from JSON DDL (data -> DDL -> Scala) for easy data mapping

## Getting Started

### Setup

For SBT simply include:
`libraryDependencies += "com.outr" %%% "fabric-core" % "x.y.z"`

For parsing support include:
`libraryDependencies += "com.outr" %%% "fabric-parse" % "x.y.z"`

### Create

Creating fabric structures with the DSL is very easy:

```scala
import fabric._

val v1 = obj(
  "name" -> "John Doe",
  "age" -> 21,
  "numbers" -> List(1, 2, 3),
  "address" -> obj(
    "street" -> "123 Somewhere Rd.",
    "city" -> "San Jose"
  )
)
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

val v3 = v1.merge(v2)
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

val backToValue: Json = person.json

case class Person(name: String, age: Int)

object Person {
  implicit val rw: RW[Person] = ccRW[Person]
}
```

### Parse

Parsing from existing JSON requires the use of the `fabric-parse` module:

```scala
import fabric._
import fabric.io._

val value = JsonParser("""{"name": "John Doe", "age": 21}""", Format.Json)
```

### Formatting

Taking an existing value and formatting it for output as JSON:

```scala
val formattedString: String = JsonFormatter.Default(value)
```