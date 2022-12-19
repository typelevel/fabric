package spec

import fabric.Json
import fabric.define.{FabricDefinition, FabricGenerator}
import fabric.io.{Format, JsonParser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.io.Source

class ComplexDefinitionSpec extends AnyWordSpec with Matchers {
  "Complex FabricDefinition" should {
    "generate complex case classes from large JSON" in {
      val json: List[Json] = JsonParser(
        Source.fromInputStream(
          getClass.getClassLoader.getResourceAsStream("large.json")
        ),
        Format.Json
      ).asVector.toList
      val dt = FabricDefinition(json)
      val generated = FabricGenerator(
        dt = dt,
        rootName = "bench.event.Event",
        (key: String) => {
          val name = "_(.)".r
            .replaceAllIn(
              key,
              m => {
                m.group(1).toUpperCase
              }
            )
            .capitalize
          s"bench.event.Event$name"
        }
      )
      generated.write(new File("bench/src/main/scala/"))
    }
  }
}
