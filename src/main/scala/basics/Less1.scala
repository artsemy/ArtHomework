package basics

import scala.annotation.tailrec

object Less1 extends App {

  def fibonacci(n: Int): Either[String, Int] = {
    @tailrec
    def count(f0: Int = 0, f1: Int = 1, number: Int = 2): Int = {
      number match {
        case x if x == n => f0 + f1
        case _ => count(f1, f0 + f1, number + 1)
      }
    }
    n match {
      case x if x < 0 => Left("Error: negative numbers aren't allowed")
      case x if x <= 1 => Right(x)
      case _ => Right(count())
    }
  }

//  //test
//  val l = for {
//    i <- (0 to 19).toList
//  } yield fibonacci2(i).getOrElse(-1)
//  println(l + " my list")
//  val l2 = List(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181)
//  println(l2 + " true list")

  def atkinSieve(n: Int): List[Int] = {
    val sqrLim = Math.sqrt(n).toInt
    val srtList = List(2, 3, 5)
    val midList = getMidList(sqrLim, n)
    val clList = delSqr(midList)
    val resList = clList.filter(x => x%3 != 0 && x%5 != 0)
    srtList ++ resList
  }

  def getMidList(sqrLim: Int, max: Int): List[Int] = {
    val l1 = for {
      x <- (1 to sqrLim).toList
      y <- (1 to sqrLim).toList
    } yield List(4*x*x + y*y, 3*x*x + y*y, 3*x*x - y*y)
    l1.flatten
      .distinct
      .filter(x => x > 0 && x <= max && (x%4 == 1 || x%6 == 1 || x%12 == 11))
      .sortWith((x, y) => x < y)
  }

  def delSqr(list: List[Int]): List[Int] = {
    list.filter(x => !list.exists(y => x / (y * y) >= 1 && x % (y * y) == 0))
    list.filter(x => !list.exists(y => list.exists(z => x == y * z)))
  }

//  println(atkinSieve(200))
}
