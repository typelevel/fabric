import scala.language.implicitConversions

package object fabric {
  implicit def string2Path(s: String): Path = new Path(List(s))

  implicit def map2Obj(map: Map[String, Value]): Obj = Obj(map)

  implicit def seq2Arr(seq: Seq[Value]): Arr = Arr(seq.toVector)

  implicit def ints2Arr(seq: Seq[Int]): Arr = Arr(seq.map(n => num(n.toDouble)).toVector)

  implicit def doubles2Arr(seq: Seq[Double]): Arr = Arr(seq.map(num).toVector)

  /**
   * Create an Obj from the params
   */
  def obj(params: (String, Value)*): Obj = Obj(Map(params: _*))

  /**
   * Create an Arr from the params
   */
  def arr(values: Value*): Arr = Arr(values.toVector)

  /**
   * Create a Str from the supplied String
   */
  implicit def str(s: String): Str = Str(s)

  /**
   * Create a Num from the supplied String
   */
  def num(value: String): Num = NumDec(BigDecimal(value))

  /**
   * Create a Num from the supplied Double
   */
  implicit def num(value: Double): Num = NumDec(BigDecimal(value))

  /**
   * Create a Num from the supplied BigDecimal
   */
  implicit def num(value: BigDecimal): Num = NumDec(value)

  implicit def num(value: Int): Num = NumInt(value.toLong)

  implicit def num(value: Long): Num = NumInt(value)

  /**
   * Create a Bool from the supplied Boolean
   */
  implicit def bool(b: Boolean): Bool = Bool(b)
}
