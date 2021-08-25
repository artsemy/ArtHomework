package adt

object Less5 {

  //european roulette
  sealed abstract class Bet(val odds: Int, val combination: RouletteNumbers)
  object Bet {
    import RouletteNumbers._
    //evens
    final case object Red extends Bet(1, RedBetNumbers)
    final case object Black extends Bet(1, BlackBetNumbers)
    final case object Even extends Bet(1, EvenBetNumbers)
    final case object Odd extends Bet(1, OddBetNumbers)
    final case object LowBet extends Bet(1, LowBetNumbers)
    final case object HighBet extends Bet(1, HighBetNumbers)
    //2 to 1
    final case object FirstDozen extends Bet(2, FirstDozenNumbers)
    final case object MiddleDozen extends Bet(2, MiddleDozenNumbers)
    final case object LastDozen extends Bet(2, LastDozenNumbers)
    final case class ColumnBet(firstNumber: Int) extends Bet(2, ColumnBetNumbers(firstNumber))
    //Longer Odds
    final case class LineBet(firstNumber: Int) extends Bet(5, LineBetNumbers(firstNumber))
    final case class CornerBet(firstNumber: Int) extends Bet(8, CornerBetNumbers(firstNumber))
    final case class StreetBet(firstNumber: Int) extends Bet(11, StreetBetNumbers(firstNumber))
    final case class SplitBet(firstNumber: Int) extends Bet(17, SplitBetNumbers(firstNumber))
    final case class StraightUp(firstNumber: Int) extends Bet(35, StreetBetNumbers(firstNumber))
  }

  sealed trait RouletteNumbers {
    def numbers: List[Int]
  }
  object RouletteNumbers {
    final case object RedBetNumbers extends RouletteNumbers { //function
      override def numbers: List[Int] = List(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
    }
    final case object BlackBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = List(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)
    }
    final case object EvenBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (2 to 36 by 2).toList
    }
    final case object OddBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (1 to 35 by 2).toList
    }
    final case object LowBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (1 to 18).toList
    }
    final case object HighBetNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (19 to 36).toList
    }
    final case object FirstDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (1 to 12).toList
    }
    final case object MiddleDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (13 to 24).toList
    }
    final case object LastDozenNumbers extends RouletteNumbers {
      override def numbers: List[Int] = (25 to 36).toList
    }
    final case class ColumnBetNumbers(columnNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (columnNumber to 36 by 3).toList
    }
    final case class LineBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (0 to 6).map(x => x + firstNumber).toList
    }
    final case class CornerBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (0 to 4).map(x => x + firstNumber).toList
    }
    final case class StreetBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (0 to 3).map(x => x + firstNumber).toList
    }
    final case class SplitBetNumbers(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (0 to 1).map(x => x + firstNumber).toList
    }
    final case class StraightUpNumber(firstNumber: Int) extends RouletteNumbers {
      override def numbers: List[Int] = (0 to 0).map(x => x + firstNumber).toList
    }
  }

  def main(args: Array[String]): Unit = {
    println("Hello world!!!")
  }

}
