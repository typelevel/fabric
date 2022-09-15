package fabric

import fabric.filter.ValueFilter

import scala.collection.immutable.ListMap
import scala.util.Try

/**
 * Value represents the base sealed trait for all representable types in fabric.
 */
sealed trait Json extends Any {
  type Type

  /**
   * Looks up a Value by name in the children.
   *
   * Throws an exception if invoked on anything except `Obj`
   */
  final def apply(lookup: String): Json = get(lookup).getOrElse(throw new RuntimeException(s"Lookup not found: $lookup"))

  /**
   * Looks up a Value by name in the children.
   */
  final def get(lookup: String): Option[Json] = this match {
    case Obj(map) => map.get(lookup)
    case _ => None
  }

  /**
   * Looks up a Value based on Path
   *
   * Example: val o: Option[Value] = someValue("first" \ "second" \ "third")
   */
  final def get(path: Path): Option[Json] = if (path.isEmpty) {
    Some(this)
  } else {
    val lookup = path()
    val next = path.next()
    get(lookup).flatMap(_.get(next))
  }

  /**
   * Looks up a Value based on Path
   *
   * Example: `val v = someValue("first" \ "second" \ "third")`
   */
  final def apply(path: Path): Json = get(path).getOrElse(throw new RuntimeException(s"Path not found: $path"))

  /**
   * Looks up a Value by name in the children or creates a new Obj if it doesn't exist.
   */
  final def getOrCreate(lookup: String): Json = get(lookup).getOrElse(obj())

  /**
   * Modifies the value at the specified path and returns back a new root Value with the modified path.
   *
   * Note: We use the term "modify" here from an immutable standpoint. The original Value will not change.
   *
   * @param path the path to modify
   * @param f the function that takes the current Value and returns the modified Value
   * @return new root Value representing the changes
   */
  def modify(path: Path)(f: Json => Json): Json = if (path.isEmpty) {
    f(this)
  } else {
    val child = this.getOrCreate(path())
    child.modify(path.next())(f) match {
      case Null => Obj(asObj.value - path())
      case v if v == child => this
      case v if isObj => Obj(asObj.value + (path() -> v))
      case v => obj(path() -> v)
    }
  }

  /**
   * Applies the filter recursively to this value beginning on the leafs working backward up the tree back to the root.
   *
   * @param filter the filter to apply
   * @return Option[Value]
   */
  def filter(filter: ValueFilter): Option[Json] = filter(this)

  /**
   * Convenience functionality for #modify to set a specific value at a path.
   *
   * @param path the path to replace
   * @param value the new value to set
   * @return new root Value representing the changes
   */
  def set(path: Path, value: Json): Json = modify(path)(_ => value)

  /**
   * Convenience functionality for #modify to remove the value at a specific path.
   *
   * @param path the path to remove
   * @return new root Value representing the changes
   */
  def remove(path: Path): Json = set(path, Null)

  /**
   * Merges a Value at the specified path
   *
   * @param value the value to merge
   * @param path the path (defaults to Path.empty)
   * @param `type` the merge type (defaults to MergeType.Overwrite)
   * @return root Value after merge
   */
  def merge(value: Json,
            path: Path = Path.empty,
            `type`: MergeType = MergeType.Overwrite): Json = modify(path) {
    case Obj(thisMap) => value match {
      case Obj(thatMap) => {
        var merged = thatMap
        thisMap.foreach {
          case (k, v) => if (merged.contains(k)) {
            `type` match {
              case MergeType.Overwrite => merged += k -> v.merge(merged(k))
              case MergeType.Add => merged += k -> merged(k).merge(v)
              case MergeType.ErrorOnDuplicate => throw new RuntimeException(s"Duplicate found: $k, existing: ${merged(k)}, new: $v")
            }
          } else {
            merged += k -> v
          }
        }
        merged
      }
      case _ => `type` match {
        case MergeType.Overwrite => value
        case MergeType.Add => Obj(thisMap)
        case MergeType.ErrorOnDuplicate => throw new RuntimeException(s"Duplicate found at $path, existing: ${Obj(thisMap)}, new: $value")
      }
    }
    case _ => value
  }

  /**
   * The type of value
   */
  def `type`: ValueType[Type]

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  /**
   * True if this is an Obj
   */
  def isObj: Boolean = `type` == ValueType.Obj

  /**
   * True if this is an Arr
   */
  def isArr: Boolean = `type` == ValueType.Arr

  /**
   * True if this is a Str
   */
  def isStr: Boolean = `type` == ValueType.Str

  /**
   * True if this is a Num
   */
  def isNum: Boolean = isNumInt || isNumDec

  def isNumInt: Boolean = `type` == ValueType.NumInt

  def isNumDec: Boolean = `type` == ValueType.NumDec

  /**
   * True if this is a Bool
   */
  def isBool: Boolean = `type` == ValueType.Bool

  /**
   * True if this is a Null
   */
  def isNull: Boolean = `type` == ValueType.Null

  /**
   * Safely casts this Value as the specified ValueType. Throws an exception if not a match.
   *
   * @param `type` the type to cast this ValueType as
   * @tparam V the return type
   */
  def asValue[V <: Json](`type`: ValueType[V]): V = if (this.`type`.is(`type`)) {
    this.asInstanceOf[V]
  } else if (`type` == ValueType.Str) {
    str(toString).asInstanceOf[V]
  } else {
    throw new RuntimeException(s"$this is a ${this.`type`}, not a ${`type`}")
  }

  /**
   * Safely casts this Value as the specified ValueType. Returns None if it's a different type.
   *
   * @param `type` the value type of value you want.
   * @tparam V the value type
   * @return Option[V]
   */
  final def getValue[V <: Json](`type`: ValueType[V]): Option[V] = if (this.`type` == `type`) {
    Some(this.asInstanceOf[V])
  } else {
    None
  }

  /**
   * Casts to Obj or throws an exception if not an Obj
   */
  def asObj: Obj = asValue[Obj](ValueType.Obj)

  /**
   * Casts to Arr or throws an exception if not an Arr
   */
  def asArr: Arr = asValue[Arr](ValueType.Arr)

  /**
   * Casts to Str or throws an exception if not a Str
   */
  def asStr: Str = asValue[Str](ValueType.Str)

  /**
   * Casts to Num or throws an exception if not a Num
   */
  def asNum: Num = asValue[Num](ValueType.Num)

  /**
   * Casts to NumInt or throws an exception if not a NumInt
   */
  def asNumInt: NumInt = asValue[NumInt](ValueType.NumInt)

  /**
   * Casts to NumDec or throws an exception if not a NumDec
   */
  def asNumDec: NumDec = asValue[NumDec](ValueType.NumDec)

  /**
   * Casts to Bool or throws an exception if not a Bool
   */
  def asBool: Bool = asValue[Bool](ValueType.Bool)

  /**
   * Casts to Obj if it's of Obj type or returns None
   */
  def getObj: Option[Obj] = getValue(ValueType.Obj)

  /**
   * Casts to Arr if it's of Arr type or returns None
   */
  def getArr: Option[Arr] = getValue(ValueType.Arr)

  /**
   * Casts to Str if it's of Str type or returns None
   */
  def getStr: Option[Str] = getValue(ValueType.Str)

  /**
   * Casts to Num if it's of Num type or returns None
   */
  def getNum: Option[Num] = getValue(ValueType.Num)

  /**
   * Casts to Bool if it's of Bool type or returns None
   */
  def getBool: Option[Bool] = getValue(ValueType.Bool)

  /**
   * Convenience method for asObj.value
   */
  def asMap: ListMap[String, Json] = asObj.value

  /**
   * Convenience method for asArr.value
   */
  def asVector: Vector[Json] = asArr.value

  /**
   * Convenience method for asStr.value
   */
  def asString: String = asStr.value

  /**
   * Convenience method for asNum.value
   */
  def asBigDecimal: BigDecimal = asNum.asBigDecimal

  def asByte: Byte = asNum.asByte
  def asShort: Short = asNum.asShort
  def asInt: Int = asNum.asInt
  def asLong: Long = asNum.asLong
  def asFloat: Float = asNum.asFloat
  def asDouble: Double = asNum.asDouble

  /**
   * Convenience method for asBool.value
   */
  def asBoolean: Boolean = asBool.value

  /**
   * Convenience method for getObj.map(_.value)
   */
  def getMap: Option[Map[String, Json]] = getObj.map(_.value)

  /**
   * Convenience method for getArr.map(_.value)
   */
  def getVector: Option[Vector[Json]] = getArr.map(_.value)

  /**
   * Convenience method for getStr.map(_.value)
   */
  def getString: Option[String] = getStr.map(_.value)

  /**
   * Convenience method for getNum.map(_.value)
   */
  def getBigDecimal: Option[BigDecimal] = getNum.map(_.asBigDecimal)

  def getByte: Option[Byte] = getNum.map(_.asByte)
  def getShort: Option[Short] = getNum.map(_.asShort)
  def getInt: Option[Int] = getNum.map(_.asInt)
  def getLong: Option[Long] = getNum.map(_.asLong)
  def getFloat: Option[Float] = getNum.map(_.asFloat)
  def getDouble: Option[Double] = getNum.map(_.asDouble)

  /**
   * Convenience method for getBool.map(_.value)
   */
  def getBoolean: Option[Boolean] = getBool.map(_.value)
}

object Json {
  /**
   * Merges multiple Values together. Convenience functionality to handle more than two Values.
   */
  def merge(values: Json*): Json = if (values.nonEmpty) {
    values.tail.foldLeft(values.head)((merged, value) => merged.merge(value))
  } else {
    Null
  }
}

/**
 * Obj represents a Map of key-value pairs (String, Value)
 */
final class Obj private(val value: ListMap[String, Json]) extends AnyVal with Json {
  override type Type = Obj

  def keys: Set[String] = value.keySet

  override def filter(filter: ValueFilter): Option[Json] = {
    val mutated = value.map {
      case (key, value) => value.filter(filter).map(v => key -> v)
    }.flatten
    filter(Obj(ListMap.from(mutated)))
  }

  override def isEmpty: Boolean = value.isEmpty

  override def `type`: ValueType[Obj] = ValueType.Obj

  override def toString: String = value.map {
    case (key, value) => s""""$key": $value"""
  }.mkString("{", ", ", "}")
}

object Obj {
  var ExcludeNullValues: Boolean = false

  val empty: Obj = Obj(ListMap.empty)

  private def clean(map: ListMap[String, Json]): ListMap[String, Json] = if (ExcludeNullValues) {
    map.filter {
      case (_, value) => value != Null
    }
  } else {
    map
  }

  def apply(value: ListMap[String, Json]): Obj = new Obj(clean(value))

  def unapply(obj: Obj): Some[ListMap[String, Json]] = Some(obj.value)

  /**
   * Processes the supplied map creating an Obj for it. If `parsePath` is set, the key will be extracted as
   * as a path based on the `parsePath` separation character. If it is not set, the key will be set as-is.
   *
   * @param map the map to parse into an Obj
   * @param parsePath the optional separation character to parse the keys into paths (defaults to Some('.'))
   * @return Obj
   */
  def process(map: Map[String, String], parsePath: Option[Char] = Some('.')): Obj = {
    var o = obj()
    map.foreach {
      case (key, value) => parsePath match {
        case Some(sep) => {
          val path = Path.parse(key, sep)
          if (path.nonEmpty) {
            o = o.merge(str(value), path).asObj
          } else {
            o = o.merge(str(value), Path("value")).asObj
          }
        }
        case None => o = o.merge(str(value), Path(key)).asObj
      }
    }
    o
  }
}

/**
 * Str represents a String
 */
case class Str(value: String) extends AnyVal with Json {
  override type Type = Str

  override def `type`: ValueType[Str] = ValueType.Str

  override def isEmpty: Boolean = value.isEmpty

  override def asValue[V <: Json](`type`: ValueType[V]): V = `type` match {
    case ValueType.Bool => Try(Bool(value.toBoolean))
      .toOption
      .map(_.asInstanceOf[V])
      .getOrElse(throw ConversionException(s"$value is a Str and can't be converted to Bool"))
    case ValueType.NumInt => Try(NumInt(value.toInt))
      .toOption
      .map(_.asInstanceOf[V])
      .getOrElse(throw ConversionException(s"$value is a Str and can't be converted to NumInt"))
    case ValueType.NumDec | ValueType.Num => Try(NumDec(BigDecimal(value)))
      .toOption
      .map(_.asInstanceOf[V])
      .getOrElse(throw ConversionException(s"$value is a Str and can't be converted to NumDec"))
    case _ => super.asValue[V](`type`)
  }

  override def toString: String = s""""${Str.escape(value)}""""
}

object Str {
  def escape(s: String): String = s.map {
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case c => c.toString
  }.mkString
}

sealed trait Num extends Any with Json {
  def asInt: Int
  def asLong: Long
  def asFloat: Float
  def asDouble: Double
  def asBigInt: BigInt
  def asBigDecimal: BigDecimal
}

/**
 * NumInt represents a numeric value and wraps a Long
 */
case class NumInt(value: Long) extends Num {
  override type Type = NumInt

  override def `type`: ValueType[NumInt] = ValueType.NumInt

  override def asValue[V <: Json](`type`: ValueType[V]): V = if (`type` == ValueType.NumDec) {
    NumDec(BigDecimal(value)).asInstanceOf[V]
  } else {
    super.asValue[V](`type`)
  }

  override def asByte: Byte = value.toByte
  override def asShort: Short = value.toShort
  override def asInt: Int = value.toInt
  override def asLong: Long = value
  override def asFloat: Float = value.toFloat
  override def asDouble: Double = value.toDouble
  override def asBigInt: BigInt = BigInt(value)
  override def asBigDecimal: BigDecimal = BigDecimal(value)

  override def isEmpty: Boolean = false

  override def equals(obj: Any): Boolean = obj match {
    case that: NumInt => this.value == that.value
    case that: NumDec => BigDecimal(value) == that.value
    case _ => false
  }

  override def toString: String = value.toString
}

/**
 * NumDec represents a numeric value and wraps a BigDecimal
 */
case class NumDec(value: BigDecimal) extends Num {
  override type Type = NumDec

  override def `type`: ValueType[NumDec] = ValueType.NumDec

  override def asValue[V <: Json](`type`: ValueType[V]): V = if (`type` == ValueType.NumInt) {
    NumInt(value.toLong).asInstanceOf[V]
  } else {
    super.asValue[V](`type`)
  }

  override def asByte: Byte = value.toByte
  override def asShort: Short = value.toShort
  override def asInt: Int = value.toInt
  override def asLong: Long = value.toLong
  override def asFloat: Float = value.toFloat
  override def asDouble: Double = value.toDouble
  override def asBigInt: BigInt = value.toBigInt
  override def asBigDecimal: BigDecimal = value

  override def isEmpty: Boolean = false

  override def equals(obj: Any): Boolean = obj match {
    case that: NumInt => this.value == BigDecimal(that.value)
    case that: NumDec => this.value == that.value
    case _ => false
  }

  override def toString: String = value.toString()
}

/**
 * Bool represents a boolean value
 */
case class Bool(value: Boolean) extends AnyVal with Json {
  override type Type = Bool

  override def `type`: ValueType[Bool] = ValueType.Bool

  override def isEmpty: Boolean = false

  override def toString: String = value.toString
}

/**
 * Arr represents an array (Vector[Value])
 */
case class Arr(value: Vector[Json]) extends AnyVal with Json {
  override type Type = Arr

  override def `type`: ValueType[Arr] = ValueType.Arr

  override def filter(filter: ValueFilter): Option[Json] = {
    val mutated = value.flatMap(v => v.filter(filter))
    filter(Arr(mutated))
  }

  override def isEmpty: Boolean = value.isEmpty

  override def toString: String = value.mkString("[", ", ", "]")
}

sealed trait Null extends Json

/**
 * Null represents a null Value
 */
object Null extends Null {
  override type Type = Null

  override def `type`: ValueType[Null] = ValueType.Null

  override def isEmpty: Boolean = true

  override def toString: String = "null"
}