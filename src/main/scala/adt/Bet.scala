package adt

import adt.Numbers.RouletteNumbers //strange import
import adt.Numbers.RouletteNumbers._


object Bet {

  //european roulette
  sealed abstract class RouletteBet(val odds: Int, val combination: RouletteNumbers)

  object RouletteBet {

    final case object RedBet extends RouletteBet(1, RedBetNumbers)
    final case object BlackBet extends RouletteBet(1, BlackBetNumbers)
    final case object EvenBet extends RouletteBet(1, EvenBetNumbers)
    final case object OddBet extends RouletteBet(1, OddBetNumbers)
    final case object LowBet extends RouletteBet(1, LowBetNumbers)
    final case object HighBet extends RouletteBet(1, HighBetNumbers)

    final case object FirstDozenBet extends RouletteBet(2, FirstDozenNumbers)
    final case object MiddleDozenBet extends RouletteBet(2, MiddleDozenNumbers)
    final case object LastDozenBet extends RouletteBet(2, LastDozenNumbers)
    final case class ColumnBet(firstNumber: Int) extends RouletteBet(2, ColumnBetNumbers(firstNumber))

    final case class LineBet(firstNumber: Int) extends RouletteBet(5, LineBetNumbers(firstNumber))
    final case class CornerBet(firstNumber: Int) extends RouletteBet(8, CornerBetNumbers(firstNumber))
    final case class StreetBet(firstNumber: Int) extends RouletteBet(11, StreetBetNumbers(firstNumber))
    final case class SplitBet(firstNumber: Int) extends RouletteBet(17, SplitBetNumbers(firstNumber))
    final case class StraightUpBet(firstNumber: Int) extends RouletteBet(35, StreetBetNumbers(firstNumber))
  }

}
