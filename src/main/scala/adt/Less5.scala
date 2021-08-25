package adt

import scala.util.Random

object Less5 {

  //european roulette
  sealed abstract class RouletteBet(val odds: Int, val combination: RouletteNumbers)
  object RouletteBet {
    import RouletteNumbers._

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

  sealed trait RouletteNumbers {
    def numbers: List[Int]
  }
  object RouletteNumbers {
    final case object RedBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] =
        fold(start = 1, end = 9, step = 2) ++
        fold(start = 12, end = 18, step = 2) ++
        fold(start = 19, end = 27, step = 2) ++
        fold(start = 30, end = 36, step = 2)
    }
    final case object BlackBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] =
        fold(start = 2, end = 10, step = 2) ++
        fold(start = 11, end = 17, step = 2) ++
        fold(start = 20, end = 28, step = 2) ++
        fold(start = 39, end = 35, step = 2)
    }
    final case object EvenBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 2, end = 36, step = 2)
    }
    final case object OddBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 1, end = 35, step = 2)
    }
    final case object LowBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 1, end = 18)
    }
    final case object HighBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 19, end = 36)
    }
    final case object FirstDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 1, end = 12)
    }
    final case object MiddleDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 13, end = 24)
    }
    final case object LastDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = 25, end = 36)
    }
    final case class ColumnBetNumbers(columnNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = fold(start = columnNumber, end = 36, step = 3)
    }
    final case class LineBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = fold(end = 5, addition = firstNumber)
    }
    final case class CornerBetNumbers(firstNumber: Int) extends RouletteNumbers { //sort???
      override def numbers: List[Int] =
        fold(start = firstNumber, end = firstNumber+3, step = 3) ++
        fold(start = firstNumber+1, end = firstNumber+1+3, step = 3)
    }
    final case class StreetBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = fold(end = 2, addition = firstNumber)
    }
    final case class SplitBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = fold(end = 1, addition = firstNumber)
    }
    final case class StraightUpNumber(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = fold(end = 0, addition = firstNumber)
    }

    private def fold(start: Int = 0, end: Int, step: Int = 1, addition: Int = 0): List[Int] = {
      (start to end by step).map(x => x + addition).toList
    }
  }

  sealed abstract case class ValidNumber private (value: Int)
  object ValidNumber {
    def create(): ValidNumber = new ValidNumber(Random.nextInt(37)) {}
  }

  final case class Chips(value: Int)
  final case class Player(name: String, id: Long)
  final case class PlayerBet(playerId: Long, bet: RouletteBet, chips: Chips)
  final case class GameResult(playerId: Long, price: Chips)

  def makeBet(playerId: Long, bet: RouletteBet, chips: Chips): PlayerBet = PlayerBet(playerId, bet, chips)
  def generateNumber(): ValidNumber = ValidNumber.create()
  def runGame(bets: List[PlayerBet], number: ValidNumber): List[GameResult] = {
    bets.map(x => betResult(x, number))
  }
  def betResult(playerBet: PlayerBet, number: ValidNumber): GameResult = {
    val bet = playerBet.bet
    val price = if (bet.combination.numbers.contains(number.value))
      Chips(playerBet.chips.value*bet.odds)
    else Chips(0)
    GameResult(playerBet.playerId, price)
  }

  import RouletteBet._

  def main(args: Array[String]): Unit = {
    val playerBets = List(PlayerBet(1l, RedBet, Chips(10)),
      PlayerBet(2l, BlackBet, Chips(20)),
      PlayerBet(3l, OddBet, Chips(30)),
      PlayerBet(4l, EvenBet, Chips(40)),
      PlayerBet(5l, LowBet, Chips(30)),
      PlayerBet(6l, LastDozenBet, Chips(20)),
      PlayerBet(7l, CornerBet(13), Chips(10)))
    val number = ValidNumber.create()
    val results = runGame(playerBets, number)
    println(number)
    println(results)
  }

}
