package hierarchical

sealed trait Value extends Any {
  def apply(path: Path): Value = if (path.isEmpty) this else throw new RuntimeException(s"$this is not an Obj. Can't lookup: $path")

  def apply(lookup: String): Value = throw new RuntimeException(s"$this is not an Obj. Can't lookup: $lookup")

  def modify(path: Path)(f: Value => Value): Value = if (path.isEmpty) {
    f(this)
  } else {
    val child = this(path())
    child.modify(path.next())(f) match {
      case Null => Obj(obj.value - path())
      case v if v == child => this
      case v => Obj(obj.value + (path() -> v))
    }
  }

  def set(path: Path, value: Value): Value = modify(path)(_ => value)

  def remove(path: Path): Value = set(path, Null)

  def `type`: ValueType

  def isObj: Boolean = `type` == ValueType.Obj
  def isArr: Boolean = `type` == ValueType.Arr
  def isStr: Boolean = `type` == ValueType.Str
  def isNum: Boolean = `type` == ValueType.Num
  def isBool: Boolean = `type` == ValueType.Bool
  def isNull: Boolean = `type` == ValueType.Null

  private def as[V <: Value](`type`: ValueType): V = if (this.`type` == `type`) {
    this.asInstanceOf[V]
  } else {
    throw new RuntimeException(s"$this is a ${this.`type`}, not a ${`type`}")
  }

  def obj: Obj = as[Obj](ValueType.Obj)
  def arr: Arr = as[Arr](ValueType.Arr)
  def str: Str = as[Str](ValueType.Str)
  def num: Num = as[Num](ValueType.Num)
  def bool: Bool = as[Bool](ValueType.Bool)
}

trait Obj extends Any with Value {
  def value: Map[String, Value]
  def keys: Set[String] = value.keySet
  def isStrict: Boolean

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

  override def `type`: ValueType = ValueType.Obj

  override def toString: String = value.map {
    case (key, value) => s"$key: $value"
  }.mkString("{", ", ", "}")
}

case class DynamicObj(value: Map[String, Value]) extends AnyVal with Obj {
  override def isStrict: Boolean = false
}

object Obj {
  def apply(value: Map[String, Value]): Obj = DynamicObj(value)
  def apply(value: Map[String, Value], keys: Set[String]): Obj = StrictObj(value, keys)
  def strict(value: Map[String, Value]): Obj = StrictObj(value, value.keySet)
}

case class StrictObj(value: Map[String, Value], override val keys: Set[String]) extends Obj {
  override def isStrict: Boolean = true
}

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

case class Num(value: Double) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Num

  override def toString: String = value.toString
}

case class Bool(value: Boolean) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Bool

  override def toString: String = value.toString
}

case class Arr(value: Vector[Value]) extends AnyVal with Value {
  override def `type`: ValueType = ValueType.Arr

  override def toString: String = value.mkString("[", ", ", "]")
}

object Null extends Value {
  override def `type`: ValueType = ValueType.Null

  override def toString: String = "null"
}