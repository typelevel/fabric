package fabric

object FabricDefinition {
  def apply(json: List[Json]): DefType = {
    var gt = apply(json.head)
    json.tail.foreach { t =>
      val g = apply(t)
      gt = gt.merge(g)
    }
    gt
  }

  def apply(json: Json): DefType = json match {
    case Obj(value) => DefType.Obj(value.map {
      case (k, v) => k -> apply(v)
    })
    case Arr(value) => DefType.Arr(apply(value.toList))
    case Str(_) => DefType.Str
    case NumInt(_) => DefType.Int
    case NumDec(_) => DefType.Dec
    case Bool(_) => DefType.Bool
    case Null => DefType.Null
  }
}