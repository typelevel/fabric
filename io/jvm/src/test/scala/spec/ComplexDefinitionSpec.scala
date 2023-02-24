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
              m => m.group(1).toUpperCase
            )
            .capitalize
          s"bench.event.Event$name"
        }
      )
      generated.write(new File("bench/src/main/scala/"))
    }
  }
}
