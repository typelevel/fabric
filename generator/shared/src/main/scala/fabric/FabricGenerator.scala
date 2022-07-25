package fabric

object FabricGenerator {
  def apply(json: List[Value]): GenType = {
    var gt = apply(json.head)
    json.tail.foreach { t =>
      val g = apply(t)
      gt = gt.merge(g)
    }
    gt
  }

  def apply(json: Value): GenType = json match {
    case Obj(value) => GenType.Obj(value.map {
      case (k, v) => k -> apply(v)
    })
    case Arr(value) => GenType.Arr(value.map(apply))
    case Str(_) => GenType.Str
    case NumInt(_) => GenType.Int
    case NumDec(_) => GenType.Dec
    case Bool(_) => GenType.Bool
    case Null => GenType.Null
  }
}

sealed trait GenType {
  def isOpt: Boolean = false
  def isNull: Boolean = false
  def opt: GenType = GenType.Opt(this)

  def merge(that: GenType): GenType = if (this == that) {
    this
  } else if (this.isNull) {
    that.opt
  } else if (that.isNull) {
    this.opt
  } else if (this.opt == that) {
    this
  } else {
    throw new RuntimeException(s"Incompatible typed: $this / $that")
  }
}

object GenType {
  case class Obj(map: Map[String, GenType]) extends GenType {
    override def merge(that: GenType): GenType = that match {
      case Obj(thatMap) => Obj(mergeMap(map, thatMap))
      case Opt(Obj(thatMap)) => Opt(Obj(mergeMap(map, thatMap)))
      case _ => super.merge(that)
    }

    private def mergeMap(m1: Map[String, GenType], m2: Map[String, GenType]): Map[String, GenType] = {
      val keys = m1.keySet ++ m2.keySet
      keys.map { key =>
        key -> m1.getOrElse(key, Null).merge(m2.getOrElse(key, Null))
      }.toMap
    }
  }
  case class Arr(types: Vector[GenType]) extends GenType
  case class Opt(t: GenType) extends GenType {
    override def isOpt: Boolean = true
    override def opt: GenType = this
  }
  case object Str extends GenType
  case object Int extends GenType
  case object Dec extends GenType
  case object Bool extends GenType
  case object Null extends GenType {
    override def isNull: Boolean = true
  }
}