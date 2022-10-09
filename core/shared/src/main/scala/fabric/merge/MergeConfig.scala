package fabric.merge

import fabric._

trait MergeConfig {
  def merge(json1: Json, json2: Json, path: Path): Json
}

object MergeConfig extends MergeConfigBuilder()