// Scala versions
val scala213 = "2.13.5"
val scala212 = "2.12.13"
val scala3 = "3.0.0-RC1"
val scala2 = List(scala213, scala212)
val allScalaVersions = scala2 //scala3 :: scala2      // Re-enable when ScalaTest support Scala.js + 3.0.0-RC1
val scalaJVMVersions = allScalaVersions
val scalaJSVersions = allScalaVersions
val scalaNativeVersions = scala2

name := "fabric"
organization in ThisBuild := "com.outr"
version in ThisBuild := "1.0.0-SNAPSHOT"
scalaVersion in ThisBuild := scala213
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")
javacOptions in ThisBuild ++= Seq("-source", "1.8", "-target", "1.8")

publishTo in ThisBuild := sonatypePublishTo.value
sonatypeProfileName in ThisBuild := "com.outr"
licenses in ThisBuild := Seq("MIT" -> url("https://github.com/outr/fabric/blob/master/LICENSE"))
sonatypeProjectHosting in ThisBuild := Some(xerial.sbt.Sonatype.GitHubHosting("outr", "fabric", "matt@outr.com"))
homepage in ThisBuild := Some(url("https://github.com/outr/fabric"))
scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/outr/fabric"),
    "scm:git@github.com:outr/fabric.git"
  )
)
developers in ThisBuild := List(
  Developer(id="darkfrog", name="Matt Hicks", email="matt@matthicks.com", url=url("http://matthicks.com"))
)

testOptions in ThisBuild += Tests.Argument("-oD")

// Dependency versions
val collectionCompatVersion: String = "2.4.2"
val scalatestVersion: String = "3.2.5"

// Parse module dependencies
val jacksonVersion: String = "2.12.1"
val scalaXMLVersion: String = "2.0.0-M5"
val typesafeConfig: String = "1.4.1"

// Benchmarks
val circeVersion: String = "0.13.0"
val uPickleVersion: String = "1.2.3"

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
      "org.scalatest" %%% "scalatest" % scalatestVersion % Test
    ),
    libraryDependencies ++= (
      if (isDotty.value) {
        Nil
      } else {
        Seq(
          "org.scala-lang.modules" %% "scala-collection-compat" % collectionCompatVersion,
          "org.scala-lang" % "scala-reflect" % scalaVersion.value
        )
      }
    ),
    Compile / unmanagedSourceDirectories ++= {
      val major = if (isDotty.value) "-3" else "-2"
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
      "org.scalatest" %%% "scalatest" % scalatestVersion % Test
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
      "org.scala-lang.modules" %% "scala-xml" % scalaXMLVersion,
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
