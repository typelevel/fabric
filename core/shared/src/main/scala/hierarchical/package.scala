import scala.language.implicitConversions

package object hierarchical {
  implicit def string2Path(s: String): Path = new Path(List(s))

  implicit def map2Obj(map: Map[String, Value]): Obj = Obj(map)

  implicit def seq2Arr(seq: Seq[Value]): Arr = Arr(seq.toVector)

  implicit def ints2Arr(seq: Seq[Int]): Arr = Arr(seq.map(n => num(n.toDouble)).toVector)

  def obj(params: (String, Value)*): Obj = Obj(Map(params: _*))

  def arr(values: Value*): Arr = Arr(values.toVector)

  implicit def str(s: String): Str = Str(s)

  implicit def num(double: Double): Num = Num(double)

  implicit def bool(b: Boolean): Bool = Bool(b)
}
