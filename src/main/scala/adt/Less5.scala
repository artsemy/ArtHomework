package adt

import adt.Bet.RouletteBet
import adt.Bet.RouletteBet._
import adt.BetUtil._

import scala.util.Random

object Less5 {

  final case class Chips(value: Int)
  final case class Player(name: String, id: Long)
  final case class PlayerBet(playerId: Long, bet: RouletteBet, chips: Chips)
  final case class GameResult(playerId: Long, price: Chips)

  sealed trait RouletteColour
  object RouletteColour {
    final case object Red extends RouletteColour
    final case object Black extends RouletteColour

    val redList = List(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)

    def getColour(num: Int): RouletteColour = {
      if (redList.contains(num)) Red else Black
    }
  }

  final case class ColoredNumber(number: Int, colour: RouletteColour)
  case object ColoredNumber {
    def apply(number: Int): ColoredNumber = {
      val colour = RouletteColour.getColour(number)
      ColoredNumber(number, colour)
    }
  }

  def generateNumber(): ColoredNumber = {
    val number = Random.nextInt(37)
    ColoredNumber(number)
  }

  def runGame(bets: List[PlayerBet], rouletteNumber: ColoredNumber): List[GameResult] = {
    bets.map(x => betResult(x, rouletteNumber))
  }

  def betResult(playerBet: PlayerBet, rouletteNumber: ColoredNumber): GameResult = {
    val isWinBet: Boolean = playerBet.bet match {
      case RouletteBet.RedBet => isRedBetWin(rouletteNumber.colour)
      case RouletteBet.BlackBet => isBlackBetWin(rouletteNumber.colour)
      case RouletteBet.EvenBet => isEvenBetWin(rouletteNumber.number)
      case RouletteBet.OddBet => !isEvenBetWin(rouletteNumber.number)
      case HalfBet(halfNumber) => isHalfBetWin(halfNumber, rouletteNumber.number)
      case DozenBet(dozenNumber) => isDozeBetWin(dozenNumber, rouletteNumber.number)
      case ColumnBet(columnNumber) => isColumnBetWin(columnNumber, rouletteNumber.number)
      case LineBet(lineNumber) => isLineBetWin(lineNumber, rouletteNumber.number)
      case CornerBet(cornerNumber) => isCornerBetWin(cornerNumber, rouletteNumber.number)
      case StreetBet(streetNumber) => isStreetBetWin(streetNumber, rouletteNumber.number)
      case SplitBet(splitNumber) => isSplitBetWin(splitNumber, rouletteNumber.number)
      case StraightUpBet(straightNumber) => isStraightUpBetWin(straightNumber, rouletteNumber.number)
      case _ => false
    }
    val prize = if (isWinBet) Chips(playerBet.bet.odds * playerBet.chips.value)
    else Chips(0)
    GameResult(playerBet.playerId, prize)
  }

  def main(args: Array[String]): Unit = {
    val playerBets = List(
      PlayerBet(1, RedBet, Chips(10)),
      PlayerBet(2, BlackBet, Chips(10)),
      PlayerBet(3, OddBet, Chips(10)),
      PlayerBet(4, EvenBet, Chips(10)),
      PlayerBet(5, HalfBet(1), Chips(10)),
      PlayerBet(6, HalfBet(2), Chips(10)),
      PlayerBet(7, DozenBet(1), Chips(10)),
      PlayerBet(8, DozenBet(2), Chips(10)),
      PlayerBet(9, DozenBet(3), Chips(10)),
      PlayerBet(10, CornerBet(13), Chips(10))
    )
    val number = generateNumber()
    val results = runGame(playerBets, number)
    println(number)
    println(results)
    playerBets.foreach(x => println(x.playerId + " " + x.bet))
  }

}
