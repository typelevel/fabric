package fabric.rw

/**
 * ClassRW provides convenience functionality to simplify class mapping with RW
 */
trait ClassRW[T] extends RW[T] with ClassR[T] with ClassW[T]