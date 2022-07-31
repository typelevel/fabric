package fabric.filter

import fabric.Json

trait ValueFilter {
  def apply(value: Json): Option[Json]
}

