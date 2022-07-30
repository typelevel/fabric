package fabric

import fabric.rw.ReaderWriter

import scala.util.Try

sealed trait DefType {
  def isOpt: Boolean = false

  def isNull: Boolean = false

  def validate(value: Value): Boolean = Try(FabricDefinition(value).merge(this)).toOption.map { d =>
    println(d)
    println(this)
    d
  }.contains(this)

  def opt: DefType = DefType.Opt(this)

  def merge(that: DefType): DefType = if (this == that) {
    this
  } else if (this.isNull) {
    that.opt
  } else if (that.isNull) {
    this.opt
  } else if (this.opt == that) {
    that
  } else {
    throw new RuntimeException(s"Incompatible typed: $this / $that")
  }
}

object DefType {
  implicit def rw: ReaderWriter[DefType] = ReaderWriter[DefType](
    r = dt2V,
    w = v2dt
  )

  private def dt2V(dt: DefType): Value = dt match {
    case Obj(map) => obj(
      "type" -> "object",
      "values" -> fabric.Obj(map.map {
        case (key, dt) => key -> dt2V(dt)
      })
    )
    case Arr(types) => obj(
      "type" -> "array",
      "values" -> fabric.Arr(types.map(dt2V))
    )
    case Opt(t) => obj(
      "type" -> "optional",
      "value" -> dt2V(t)
    )
    case Str => obj(
      "type" -> "string"
    )
    case Int => obj(
      "type" -> "numeric",
      "precision" -> "integer"
    )
    case Dec => obj(
      "type" -> "numeric",
      "precision" -> "decimal"
    )
    case Bool => obj(
      "type" -> "boolean"
    )
    case Null => obj(
      "type" -> "null"
    )
  }

  private def v2dt(v: Value): DefType = {
    val o = v.asObj
    o.value("type").asString match {
      case "object" => Obj(o.value("values").asMap.map {
        case (key, value) => key -> v2dt(value)
      })
      case "array" => Arr(o.value("values").asVector.map(v2dt))
      case "optional" => Opt(v2dt(o.value("value")))
      case "string" => Str
      case "numeric" => o.value("precision").asString match {
        case "integer" => Int
        case "decimal" => Dec
      }
      case "boolean" => Bool
      case "null" => Null
    }
  }

  case class Obj(map: Map[String, DefType]) extends DefType {
    override def merge(that: DefType): DefType = that match {
      case Obj(thatMap) => Obj(mergeMap(map, thatMap))
      case Opt(Obj(thatMap)) => Opt(Obj(mergeMap(map, thatMap)))
      case _ => super.merge(that)
    }

    private def mergeMap(m1: Map[String, DefType], m2: Map[String, DefType]): Map[String, DefType] = {
      val keys = m1.keySet ++ m2.keySet
      keys.map { key =>
        key -> m1.getOrElse(key, Null).merge(m2.getOrElse(key, Null))
      }.toMap
    }
  }
  case class Arr(types: Vector[DefType]) extends DefType
  case class Opt(t: DefType) extends DefType {
    override def isOpt: Boolean = true
    override def opt: DefType = this
  }
  case object Str extends DefType
  case object Int extends DefType
  case object Dec extends DefType
  case object Bool extends DefType
  case object Null extends DefType {
    override def isNull: Boolean = true
  }
}