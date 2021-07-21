package fabric.rw

/**
 * ClassRW provides convenience functionality to simplify class mapping with ReaderWriter
 */
trait ClassRW[T] extends ReaderWriter[T] with ClassR[T] with ClassW[T]