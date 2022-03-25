// Scala versions
val scala213 = "2.13.8"
val scala212 = "2.12.15"
val scala3 = List("3.1.1")
val scala2 = List(scala213, scala212)
val allScalaVersions = scala3 ::: scala2
val scalaJVMVersions = allScalaVersions
val scalaJSVersions = allScalaVersions
val scalaNativeVersions = scala2

name := "fabric"
ThisBuild / organization := "com.outr"
ThisBuild / version := "1.2.9"
ThisBuild / scalaVersion := scala213
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")
ThisBuild / javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / sonatypeProfileName := "com.outr"
ThisBuild / licenses := Seq("MIT" -> url("https://github.com/outr/fabric/blob/master/LICENSE"))
ThisBuild / sonatypeProjectHosting := Some(xerial.sbt.Sonatype.GitHubHosting("outr", "fabric", "matt@outr.com"))
ThisBuild / homepage := Some(url("https://github.com/outr/fabric"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/outr/fabric"),
    "scm:git@github.com:outr/fabric.git"
  )
)
ThisBuild / developers := List(
  Developer(id="darkfrog", name="Matt Hicks", email="matt@matthicks.com", url=url("https://matthicks.com"))
)

// Dependency versions
val collectionCompatVersion: String = "2.7.0"
val scalaTestVersion: String = "3.2.11"
val scalaCheckVersion: String = "3.2.11.0"

// Parse module dependencies
val jacksonVersion: String = "2.13.2"
val typesafeConfig: String = "1.4.2"

// Benchmarks
val circeVersion: String = "0.13.0"
val uPickleVersion: String = "1.3.12"

// set source map paths from local directories to github path
val sourceMapSettings = List(
  scalacOptions ++= git.gitHeadCommit.value.map { headCommit =>
    val local = baseDirectory.value.toURI
    val remote = s"https://raw.githubusercontent.com/outr/fabric/$headCommit/"
    s"-P:scalajs:mapSourceURI:$local->$remote"
  }
)

lazy val root = project.in(file("."))
  .aggregate(
    core.js, core.jvm, core.native, parse.js, parse.jvm
  )
  .settings(
    name := "fabric",
    publish := {},
    publishLocal := {}
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "fabric-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalatestplus" %%% "scalacheck-1-15" % scalaCheckVersion % Test
    ),
    libraryDependencies ++= (
      if (scalaVersion.value.startsWith("3")) {
        Nil
      } else {
        Seq(
          "org.scala-lang.modules" %% "scala-collection-compat" % collectionCompatVersion,
          "org.scala-lang" % "scala-reflect" % scalaVersion.value
        )
      }
    ),
    Compile / unmanagedSourceDirectories ++= {
      val major = if (scalaVersion.value.startsWith("3")) "-3" else "-2"
      List(CrossType.Pure, CrossType.Full).flatMap(
        _.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + major))
      )
    }
  )
  .jsSettings(
    crossScalaVersions := scalaJSVersions
  )
  .jvmSettings(
    crossScalaVersions := scalaJVMVersions
  )
  .nativeSettings(
    scalaVersion := scala213,
    crossScalaVersions := scalaNativeVersions
  )

lazy val parse = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "fabric-parse",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % scalaCheckVersion % Test
    )
  )
  .jsSettings(
    crossScalaVersions := scalaJSVersions
  )
  .jvmSettings(
    crossScalaVersions := scalaJVMVersions,
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-xml" % jacksonVersion,
      "com.jsoniter" % "jsoniter" % "0.9.23",
      "com.typesafe" % "config" % typesafeConfig
    )
  )
  .dependsOn(core)

lazy val bench = project.in(file("bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(parse.jvm)
  .settings(
    name := "fabric-benchmarks",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "com.lihaoyi" %% "upickle" % uPickleVersion
    )
  )
