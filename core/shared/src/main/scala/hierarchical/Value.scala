package hierarchical

/**
 * Value represents the base sealed trait for all representable types in hierarchical.
 */
sealed trait Value extends Any {
  /**
   * Looks up a Value based on Path
   *
   * Example: `val v = someValue("first" \ "second" \ "third")`
   */
  def apply(path: Path): Value = if (path.isEmpty) this else throw new RuntimeException(s"$this is not an Obj. Can't lookup: $path")

  /**
   * Looks up a Value by name in the children.
   *
   * Throws an exception if invoked on anything except `Obj`
   */
  def apply(lookup: String): Value = throw new RuntimeException(s"$this is not an Obj. Can't lookup: $lookup")

  /**
   * Modifies the value at the specified path and returns back a new root Value with the modified path.
   *
   * Note: We use the term "modify" here from an immutable standpoint. The original Value will not change.
   *
   * @param path the path to modify
   * @param f the function that takes the current Value and returns the modified Value
   * @return new root Value representing the changes
   */
  def modify(path: Path)(f: Value => Value): Value = if (path.isEmpty) {
    f(this)
  } else {
    val child = this(path())
    child.modify(path.next())(f) match {
      case Null => Obj(asObj.value - path())
      case v if v == child => this
      case v => Obj(asObj.value + (path() -> v))
    }
  }

  /**
   * Convenience functionality for #modify to set a specific value at a path.
   *
   * @param path the path to replace
   * @param value the new value to set
   * @return new root Value representing the changes
   */
  def set(path: Path, value: Value): Value = modify(path)(_ => value)

  /**
   * Convenience functionality for #modify to remove the value at a specific path.
   *
   * @param path the path to remove
   * @return new root Value representing the changes
   */
  def remove(path: Path): Value = set(path, Null)

  /**
   * Merges two Value instances with `that` replacing values of the same name in `this`.
   *
   * @param that the Value to merge into this
   * @return merged Value
   */
  def merge(that: Value): Value = that

  /**
   * Merges a Value at the specified path.
   *
   * @param path the path to merge into the specified path
   * @param value the value to merge
   * @return root Value after merge
   */
  def merge(path: Path, value: Value): Value = modify(path)(_.merge(value))

  /**
   * The type of value
   */
  def `type`: ValueType

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
  def isNum: Boolean = `type` == ValueType.Num

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
  def asValue[V <: Value](`type`: ValueType): V = if (this.`type` == `type`) {
    this.asInstanceOf[V]
  } else {
    throw new RuntimeException(s"$this is a ${this.`type`}, not a ${`type`}")
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
   * Casts to Bool or throws an exception if not a Bool
   */
  def asBool: Bool = asValue[Bool](ValueType.Bool)
}

object Value {
  /**
   * Merges multiple Values together. Convenience functionality to handle more than two Values.
   */
  def merge(values: Value*): Value = if (values.nonEmpty) {
    values.tail.foldLeft(values.head)((merged, value) => merged.merge(value))
  } else {
    Null
  }
}

/**
 * Obj represents a Map of key-value pairs (String, Value)
 */
case class Obj(value: Map[String, Value]) extends AnyVal with Value {
  def keys: Set[String] = value.keySet

  override def apply(path: Path): Value = if (path.isEmpty) {
    this
  } else {
    val child = this(path())
    child(path.next())
  }
  override def apply(lookup: String): Value = value.get(lookup) match {
    case Some(v) => v
    case None => throw new RuntimeException(s"Unable to find: $lookup in $this")
  }

  override def merge(that: Value): Value = that match {
    case Obj(thatMap) => {
      var merged = thatMap
      value.foreach {
        case (key, value) => if (merged.contains(key)) {
          merged += key -> value.merge(merged(key))
        } else {
          merged += key -> value
        }
      }
      merged
    }
    case _ => that
  }

  override def `type`: ValueType = ValueType.Obj

  override def toString: String = value.map {
    case (key, value) => s""""$key": $value"""
  }.mkString("{", ", ", "}")
}

/**
 * Str represents a String
 */
case class Str(value: String) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Str

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

/**
 * Num represents a numeric value and wraps a BigDecimal
 */
case class Num(value: BigDecimal) extends AnyVal with Value {
  def asShort: Short = value.toShort
  def asInt: Int = value.toInt
  def asLong: Long = value.toLong
  def asFloat: Float = value.toFloat
  def asDouble: Double = value.toDouble
  def asBigInt: BigInt = value.toBigInt
  def asBigDecimal: BigDecimal = value

  override def `type`: ValueType = ValueType.Num

  override def toString: String = value.toString
}

/**
 * Bool represents a boolean value
 */
case class Bool(value: Boolean) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Bool

  override def toString: String = value.toString
}

/**
 * Arr represents an array (Vector[Value])
 */
case class Arr(value: Vector[Value]) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Arr

  override def toString: String = value.mkString("[", ", ", "]")
}

/**
 * Null represents a null Value
 */
object Null extends Value {
  override def `type`: ValueType = ValueType.Null

  override def toString: String = "null"
}