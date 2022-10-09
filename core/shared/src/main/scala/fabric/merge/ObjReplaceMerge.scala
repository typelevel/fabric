package fabric.merge

import fabric.{MergeType, Obj, Path}

object ObjReplaceMerge extends JsonMerge[Obj] {
  override def merge(path: Path, json1: Obj, json2: Obj, config: MergeConfig): Obj = json2
}