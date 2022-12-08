package util

import java.nio.file.{Files, Paths}
import sys.process._

object UpdateReadme {
  def main(args: Array[String]): Unit = {
    val version = if (args.length == 0) {
      nextVersion()
    } else if (args.length == 1) {
      Version(args.head)
    } else {
      println("Must have either zero or one argument (version override)")
      sys.exit(1)
    }
    updateReadme(version)
    gitCommit(version)
    gitTag(version)
  }

  private def nextVersion(): Version = {
    val result = "git tag".!!
    result
      .split('\n')
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .filter(_.startsWith("v"))
      .map(Version.apply)
      .max
      .next
  }

  private def gitCommit(version: Version): Unit = {
    s"git commit -a -m \"Release $version\"".! match {
      case 0 => // Success
      case n => throw new RuntimeException(s"Git Commit Failure: $n")
    }
  }

  private def gitTag(version: Version): Unit = {
    s"git tag $version".! match {
      case 0 => // Success
      case n => throw new RuntimeException(s"Git Tag Failure: $n")
    }
  }

  private def updateReadme(version: Version): Unit = {
    val readme = Paths.get("README.md")
    val text = new String(Files.readAllBytes(readme), "UTF-8")
      .replace("$VERSION", version.number)
    Files.write(readme, text.getBytes("UTF-8"))
    ()
  }

  private case class Version(major: Int, minor: Int, build: Int) extends Ordered[Version] {
    def next: Version = copy(build = build + 1)

    override def compare(that: Version): Int = this.major - that.major match {
      case n if n != 0 => n
      case _ => this.minor - that.minor match {
        case n if n != 0 => n
        case _ => this.build - that.build
      }
    }

    lazy val number: String = s"$major.$minor.$build"

    override def toString: String = s"v$number"
  }

  private object Version {
    private val Regex = """v?(\d+)[.](\d+)[.](\d+)""".r

    def apply(version: String): Version = version match {
      case Regex(major, minor, build) => Version(major.toInt, minor.toInt, build.toInt)
      case _ => throw new RuntimeException(s"Invalid version: $version")
    }
  }
}
