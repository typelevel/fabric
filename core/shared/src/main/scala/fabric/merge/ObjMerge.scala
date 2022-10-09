package fabric.merge

import fabric.{MergeType, Obj, Path}

object ObjMerge extends JsonMerge[Obj] {
  override def merge(path: Path, json1: Obj, json2: Obj, config: MergeConfig): Obj = {
    var merged = json2.value
    json1.value.foreach {
      case (k, v) => if (merged.contains(k)) {
        merged += k -> config.merge(v, merged(k), path \ k)
      } else {
        merged += k -> v
      }
    }
    Obj(merged)
  }
}
