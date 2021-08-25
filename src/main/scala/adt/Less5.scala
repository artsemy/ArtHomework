package adt

import Bet.RouletteBet

import scala.util.Random

object Less5 {

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
    val prize = if (bet.combination.numbers.contains(number.value))
      Chips(playerBet.chips.value*bet.odds)
    else Chips(0)
    GameResult(playerBet.playerId, prize)
  }

  import RouletteBet._

  def main(args: Array[String]): Unit = {
    val playerBets = List(
      PlayerBet(1, RedBet, Chips(10)),
      PlayerBet(2, BlackBet, Chips(20)),
      PlayerBet(3, OddBet, Chips(30)),
      PlayerBet(4, EvenBet, Chips(40)),
      PlayerBet(5, LowBet, Chips(30)),
      PlayerBet(6, LastDozenBet, Chips(20)),
      PlayerBet(7, CornerBet(13), Chips(10))
    )
    val number = ValidNumber.create()
    val results = runGame(playerBets, number)
    println(number)
    println(results)
    playerBets.foreach(x => println(x.playerId + " " + x.bet.combination.numbers))
  }

}
