package fabric

/**
 * MergeType is used to determine how merging of two Values should occur
 */
sealed trait MergeType

object MergeType {
  /**
   * Standard merge. New values overwrite on collision.
   */
  case object Overwrite extends MergeType

  /**
   * Default values. New values are ignored on collision and the existing value is retained.
   */
  case object Add extends MergeType

  /**
   * Throws an exception on collision.
   */
  case object ErrorOnDuplicate extends MergeType
}