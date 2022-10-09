package fabric.merge

import fabric.{Arr, MergeType, Path}

object ArrConcatDedupeMerge extends JsonMerge[Arr] {
  override def merge(path: Path, json1: Arr, json2: Arr, config: MergeConfig): Arr = {
    val v = json1.value ++ json2.value
    Arr(v.distinct)
  }
}
