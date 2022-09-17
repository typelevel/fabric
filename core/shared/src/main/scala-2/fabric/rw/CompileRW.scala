package fabric.rw

import scala.language.experimental.macros

trait CompileRW {
  def ccRW[T]: RW[T] = macro RWMacros.caseClassRW[T]
  def ccR[T]: Reader[T] = macro RWMacros.caseClassR[T]
  def ccW[T]: Writer[T] = macro RWMacros.caseClassW[T]
}