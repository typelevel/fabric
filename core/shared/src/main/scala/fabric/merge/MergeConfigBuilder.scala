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

package fabric.merge

import fabric.{Arr, Json, JsonPath, MergeType, Obj}

case class MergeConfigBuilder(
  `type`: MergeType = MergeType.Overwrite,
  defaultArr: JsonMerge[Arr] = ArrReplaceMerge,
  defaultObj: JsonMerge[Obj] = ObjMerge,
  overrides: Map[JsonPath, JsonMerge[Json]] = Map.empty
) extends MergeConfig {
  override def merge(json1: Json, json2: Json, path: JsonPath): Json =
    overrides.get(path) match {
      case Some(merge) => merge.merge(path, json1, json2, this)
      case None if json1.`type` != json2.`type` =>
        `type` match {
          case MergeType.Overwrite => json2
          case MergeType.Add => json1
          case MergeType.ErrorOnDuplicate =>
            throw new RuntimeException(
              s"Cannot merge different types: $json1 -> $json2 ($path)"
            )
        }
      case None =>
        if (json1.isArr) {
          defaultArr.merge(path, json1.asArr, json2.asArr, this)
        } else if (json1.isObj) {
          defaultObj.merge(path, json1.asObj, json2.asObj, this)
        } else {
          `type` match {
            case MergeType.Overwrite => json2
            case MergeType.Add => json1
            case MergeType.ErrorOnDuplicate =>
              throw new RuntimeException(
                s"Duplicate found at $path, existing: $json1, new: $json2"
              )
          }
        }
    }

  def withOverride[T <: Json](
    path: JsonPath,
    jsonMerge: JsonMerge[T]
  ): MergeConfigBuilder = copy(
    overrides =
      overrides + (path -> jsonMerge.asInstanceOf[JsonMerge[Json]])
  )
}
