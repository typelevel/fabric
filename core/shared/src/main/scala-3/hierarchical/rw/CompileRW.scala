package hierarchical.rw

trait CompileRW {
  inline def ccRW[T]: ReadableWritable[T] = ${RWMacros.caseClass[T]}
}