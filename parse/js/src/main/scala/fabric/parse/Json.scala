package fabric.parse

import fabric._

import scala.scalajs.js
import scala.scalajs.js.JSON

/**
 * Json provides convenience functionality to parse and format JSON to/from fabric Values
 */
object Json extends AbstractJson {
  override def parse(s: String): Value = parse(JSON.parse(s))

  def parse(value: js.Any): Value = value.asInstanceOf[Any] match {
    case null => Null
    case v: js.Array[_] => Arr(v.toVector.map(a => parse(a.asInstanceOf[js.Any])))
    case v: js.BigInt => num(v.toString())
    case v: js.Object => {
      val d = v.asInstanceOf[js.Dictionary[js.Any]]
      d.toMap.map {
        case (key, value) => key -> parse(value)
      }
    }
    case v: String => str(v)
    case v: Boolean => bool(v)
    case v: Byte => num(v.doubleValue())
    case v: Float => num(v.toDouble)
    case v => throw new RuntimeException(s"Unsupported value in parse: $v (${v.getClass})")
  }
}
