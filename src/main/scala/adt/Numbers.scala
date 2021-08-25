package adt

object Numbers {

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

}
