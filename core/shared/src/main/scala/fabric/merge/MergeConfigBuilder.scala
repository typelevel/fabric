package fabric.merge

import fabric.{Arr, Json, MergeType, Obj, Path}

case class MergeConfigBuilder(`type`: MergeType = MergeType.Overwrite,
                              defaultArr: JsonMerge[Arr] = ArrReplaceMerge,
                              defaultObj: JsonMerge[Obj] = ObjMerge,
                              overrides: Map[Path, JsonMerge[Json]] = Map.empty) extends MergeConfig {
  override def merge(json1: Json, json2: Json, path: Path): Json = {
    overrides.get(path) match {
      case Some(merge) => merge.merge(path, json1, json2, this)
      case None if json1.`type` != json2.`type` => `type` match {
        case MergeType.Overwrite => json2
        case MergeType.Add => json1
        case MergeType.ErrorOnDuplicate => throw new RuntimeException(s"Cannot merge different types: $json1 -> $json2 ($path)")
      }
      case None => if (json1.isArr) {
        defaultArr.merge(path, json1.asArr, json2.asArr, this)
      } else if (json1.isObj) {
        defaultObj.merge(path, json1.asObj, json2.asObj, this)
      } else {
        `type` match {
          case MergeType.Overwrite => json2
          case MergeType.Add => json1
          case MergeType.ErrorOnDuplicate => throw new RuntimeException(s"Duplicate found at $path, existing: $json1, new: $json2")
        }
      }
    }
  }

  def withOverride[T <: Json](path: Path, jsonMerge: JsonMerge[T]): MergeConfigBuilder = copy(overrides = overrides + (path -> jsonMerge.asInstanceOf[JsonMerge[Json]]))
}
