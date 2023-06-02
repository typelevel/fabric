/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package fabric.define

import fabric.{obj, Json, JsonPath}

case class StaticTemplateConfig(
  s: String = "",
  i: Int = 0,
  d: BigDecimal = BigDecimal(0),
  b: Boolean = true,
  dyn: Json = obj()
) extends TemplateConfig {
  override def string(path: JsonPath): String = s

  override def int(path: JsonPath): Int = i

  override def dec(path: JsonPath): BigDecimal = d

  override def bool(path: JsonPath): Boolean = b

  override def dynamic(path: JsonPath): Json = dyn

  override def `enum`(path: JsonPath, values: List[Json]): Json = values.head
}
