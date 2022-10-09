package fabric.merge

import fabric.{Json, MergeType, Path}

trait JsonMerge[T <: Json] {
  def merge(path: Path, json1: T, json2: T, config: MergeConfig): T
}