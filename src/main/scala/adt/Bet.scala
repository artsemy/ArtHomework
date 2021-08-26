package adt

object Bet {

  //european roulette
  sealed abstract class RouletteBet(val odds: Int)

  object RouletteBet {

    final case object RedBet extends RouletteBet(1)
    final case object BlackBet extends RouletteBet(1)
    final case object EvenBet extends RouletteBet(1)
    final case object OddBet extends RouletteBet(1)
    final case class HalfBet(halfNumber: Int) extends RouletteBet(1)

    final case class DozenBet(dozenNumber: Int) extends RouletteBet(2)
    final case class ColumnBet(columnNumber: Int) extends RouletteBet(2)

    final case class LineBet(lineNumber: Int) extends RouletteBet(5)
    final case class CornerBet(cornerNumber: Int) extends RouletteBet(8)
    final case class StreetBet(streetNumber: Int) extends RouletteBet(11)
    final case class SplitBet(splitNumber: Int) extends RouletteBet(17)
    final case class StraightUpBet(straightNumber: Int) extends RouletteBet(35)
  }

}
