package fabric.rw

import scala.language.experimental.macros

trait CompileRW {
  def gen[T]: RW[T] = macro RWMacros.caseClassRW[T]
  def genR[T]: Reader[T] = macro RWMacros.caseClassR[T]
  def genW[T]: Writer[T] = macro RWMacros.caseClassW[T]
}