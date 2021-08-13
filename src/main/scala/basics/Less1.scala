package basics

import scala.annotation.tailrec

object Less1 extends App {

  def fibonacci1(n: Int): Either[String, Int] = {
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

  //not good, arguments access
  //def fibonacci3(n: Int): Either[String, Int] = {fibonacci2(n)} //private fibonacci2
  @tailrec
  def fibonacci2(n: Int, f0: Int = 0, f1: Int = 1, number: Int = 2): Either[String, Int] = {
    (n, number) match {
      case (x, _) if x < 0 => Left("Error: negative numbers aren't allowed")
      case (x, _) if x <= 1 => Right(x)
      case (x, y) if x == y => Right(f0 + f1)
      case (x, y) => fibonacci2(x, f1, f0+f1, y+1)
    }
  }



//  //test
//  val l1 = for {i <- (0 to 19).toList} yield fibonacci1(i).getOrElse(-1)
//  val l2 = for {i <- (0 to 19).toList} yield fibonacci2(i).getOrElse(-1)
//  println(l1 + " my list1")
//  println(l2 + " my list2")
//  val l3 = List(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181)
//  println(l3 + " true list")



  def atkinSieve(upperBorder: Int): List[Int] = {
    val initList = List(2, 3, 5)
    val unfilteredList = unfilteredPrimeNumberList(upperBorder)
    val filteredList = filterPrimeNumberList(unfilteredList)
    initList ++ filteredList
  }

  def unfilteredPrimeNumberList(upperBorder: Int): List[Int] = {
    val squareBorder = Math.sqrt(upperBorder).toInt
    val unfilteredList = for {
      x <- (1 to squareBorder).toList
      y <- (1 to squareBorder).toList
    } yield List(4*x*x + y*y, 3*x*x + y*y, 3*x*x - y*y)
    unfilteredList.flatten
      .distinct
      .filter(x => x > 0 && x <= upperBorder && (x%4 == 1 || x%6 == 1 || x%12 == 11)) //move to filterPrimeNumberList?
      .sortWith((x, y) => x < y)
  }

  def filterPrimeNumberList(list: List[Int]): List[Int] = {
    val res = list.filter(x => !list.exists(y => x / (y * y) >= 1 && x % (y * y) == 0))
      .filter(x => !list.exists(y => list.exists(z => x == y * z)))
      .filter(x => x%3 != 0 && x%5 != 0)
    val s1 = for { //  = list.flatMap(x => list.map(y => y*x))
      x <- list
      y <- list
    } yield x*y
    list.filterNot(x => s1.contains(x))
    res
  }

  //test
  val primeCheckList = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
    53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139,
    149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199)
  val myPrimList = atkinSieve(200)
  println(primeCheckList == myPrimList)
}
