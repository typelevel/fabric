// Scala versions
val scala213 = "2.13.9"
val scala3 = List("3.2.0")
val scala2 = List(scala213)
val scalaVersions = scala3 ::: scala2

name := "fabric"
ThisBuild / organization := "com.outr"
ThisBuild / version := "1.6.1"
ThisBuild / scalaVersion := scala213
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")
ThisBuild / javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
ThisBuild / crossScalaVersions := scalaVersions

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / publishTo := sonatypePublishToBundle.value
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
val collectionCompatVersion: String = "2.8.1"

val scalaTestVersion: String = "3.2.14"

val scalaCheckVersion: String = "3.2.14.0"

// Parse module dependencies
val jacksonVersion: String = "2.13.4"
val typesafeConfigVersion: String = "1.4.2"
val jsoniterJavaVersion: String = "0.9.23"

// Benchmarks
val circeVersion: String = "0.14.2"
val uPickleVersion: String = "2.0.0"

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
    core.js, core.jvm, core.native, io.js, io.jvm, define.jvm
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
      "org.scalatestplus" %%% "scalacheck-1-16" % scalaCheckVersion % Test
    ),
    libraryDependencies ++= (
      if (scalaVersion.value.startsWith("3")) {
        Nil
      } else {
        Seq(
          "org.scala-lang.modules" %%% "scala-collection-compat" % collectionCompatVersion,
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

lazy val io = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "fabric-io",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-16" % scalaCheckVersion % Test
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-xml" % jacksonVersion,
      "com.jsoniter" % "jsoniter" % jsoniterJavaVersion,
      "com.typesafe" % "config" % typesafeConfigVersion
    )
  )
  .dependsOn(core)

lazy val define = crossProject(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "fabric-define",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-16" % scalaCheckVersion % Test
    )
  )
  .dependsOn(io)

lazy val bench = project.in(file("bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(io.jvm)
  .settings(
    name := "fabric-benchmarks",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "com.lihaoyi" %% "upickle" % uPickleVersion
    )
  )

lazy val docs = project
  .in(file("documentation"))
  .dependsOn(io.jvm)
  .enablePlugins(MdocPlugin)
  .settings(
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    mdocOut := file(".")
  )