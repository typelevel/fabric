package hierarchical.rw

import scala.language.experimental.macros

trait CompileRW {
  def ccRW[T]: ReaderWriter[T] = macro RWMacros.caseClass[T]
}