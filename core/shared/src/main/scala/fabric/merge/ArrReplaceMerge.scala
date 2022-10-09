package fabric.merge

import fabric.{Arr, MergeType, Path}

object ArrReplaceMerge extends JsonMerge[Arr] {
  override def merge(path: Path, json1: Arr, json2: Arr, config: MergeConfig): Arr = json2
}
