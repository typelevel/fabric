package hierarchical.rw

import scala.language.experimental.macros

trait CompileRW {
  def ccRW[T]: ReadableWritable[T] = macro RWMacros.caseClass[T]
}