package tf.domain

object workingPosition {

  sealed trait WorkingPosition

  object WorkingPosition {
    final case object Junior extends WorkingPosition
    final case object Middle extends WorkingPosition
    final case object Senior extends WorkingPosition
  }

}
