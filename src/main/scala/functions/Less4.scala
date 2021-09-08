package functions

import scala.annotation.tailrec

object Less4 {

  //https://www.codewars.com/kata/555eded1ad94b00403000071/train/scala
  object Sol {

    def seriesSum(n: Int): String = {

      @tailrec
      def countSeries(currentStep: Int, divider: Int, sum: Double): Double = {
        if (currentStep < n) countSeries(currentStep + 1, divider + 3, sum + 1/divider.toDouble)
        else sum
      }

      val counted = countSeries(0, 1, 0)
      counted.formatted("%.2f")
    }
  }

  //https://www.codewars.com/kata/525f50e3b73515a6db000b83/train/scala
  object Kata {
    def createPhoneNumber(numbers: Seq[Int]): String = {
      val number = numbers.map(x => x.toString).reduce((x, y) => x + y)
      "(%s) %s-%s".format(number.substring(0, 3), number.substring(3, 6), number.substring(6, 10))
    }
  }

  //https://www.codewars.com/kata/526571aae218b8ee490006f4/train/scala
  object Kata2 {

    def countBits(n: Int): Int = {
      @tailrec
      def count(m: Int, sum: Int): Int = {
        if (m == 0) sum
        else count(m/2, sum + m%2)
      }
      count(n, 0)
    }
  }

  //https://www.codewars.com/kata/550498447451fbbd7600041c/train/scala
  object Solution {
    def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean = {
      if (seq1 == null || seq2 == null) false
      else seq1.map(x => x*x).sorted == seq2.sorted
    }
  }

  //https://www.codewars.com/kata/57eb8fcdf670e99d9b000272/train/scala
  object Scoring {
    def high(s: String): String = {
      def count(str: String): Int = str.toCharArray.foldLeft(0)((acc, x) => acc + x.toInt - 96)
      s.split("[^a-z]+").sortWith((s1, s2) => count(s1) > count(s2)).head
    }
  }

  //https://www.codewars.com/kata/51ba717bb08c1cd60f00002f/train/scala
  object Kata3 {
    def solution(xs: List[Int]): String = {

      @tailrec
      def build(str: String, list: List[Int]): String = {
        list match {
          case first :: second :: tail if second - first == 1 => build(str + first + "H", second :: tail)
          case first :: second :: tail => build(str + first + ",", second :: tail)
//          case first :: second :: Nil if second - first == 1 => str + first + "H" + second
          case first :: Nil => str + first
          case Nil => ""
        }
      }

      val startLine = build("", xs)
      val processLine1 = startLine.replaceAll("H(-?)\\d+H((-?)\\d+H)*", "K")
      val processLine2 = processLine1.replaceAll("K", "-")
      processLine2.replaceAll("H", ",")
    }
  }

  //https://www.codewars.com/kata/556deca17c58da83c00002db/train/scala
  object Tribonacci {
    def tribonacci[T : Numeric](signature: List[T], n: Int): List[T] = {
      n match {
        case x if x < 4 => signature.slice(0, n)
        case x => signature.head +: tribonacci(signature.tail :+ signature.sum, x-1)
      }
    }
  }

  //https://www.codewars.com/kata/529bf0e9bdf7657179000008/train/scala
  object Sudoku {
    val validArray: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

    def isValid(board: Array[Array[Int]]): Boolean = {
      val transposedBoard = transposeBoard(board)
      checkRows(board) && checkRows(transposedBoard) && checkBoxes(board, true)
    }

    def checkRows(board: Array[Array[Int]]): Boolean = {
      board.forall(x => x.sorted sameElements validArray)
    }

    @tailrec
    def checkBoxes(board: Array[Array[Int]], flag: Boolean): Boolean = {
      board.length match {
        case 0 => flag
        case _ => val res = checkLineBoxes(board.take(3), true)
          checkBoxes(board.slice(3, board.length), res && flag)
      }
    }

    def checkLineBoxes(line: Array[Array[Int]], flag: Boolean): Boolean = {
      val box1 = line.flatMap(x => x.take(3)).sorted sameElements validArray
      val box2 = line.flatMap(x => x.slice(3, 6)).sorted sameElements validArray
      val box3 = line.flatMap(x => x.slice(6, 10)).sorted sameElements validArray
      box1 && box2 && box3
    }

    @tailrec
    def transposeBoard(initBoard: Array[Array[Int]], resultBoard: Array[Array[Int]] = Array.empty): Array[Array[Int]] = {
      initBoard.head.length match {
        case 0 => resultBoard
        case _ =>
          val column = initBoard.map(x => x.head)
          val restMatrix = initBoard.map(x => x.tail)
          transposeBoard(restMatrix, resultBoard :+ column)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val validBoard = Array(
      Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
      Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
      Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
      Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
      Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
      Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
      Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
      Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
      Array(3, 4, 5, 2, 8, 6, 1, 7, 9)
    )
    println(Sudoku.isValid(validBoard))
  }

}
