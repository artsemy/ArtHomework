package basics

object Less1 extends App {

  def fibonacci(n: Int): Either[String, Int] = {
    n match {
      case x if x < 0 => Left("Error message")
      case 0 => Right(0)
      case 1 => Right(1)
      case y if y > 1 => if (fibonacci(y-1).isRight && fibonacci(y-2).isRight) {
        Right(fibonacci(y-1).getOrElse(-1) + fibonacci(y-2).getOrElse(-1))
      } else Left("Super magic")
      case _ => Left("Magic here")
    }
  }

  def atkinSieve(n: Int): List[Int] = {
    val extraList1 = List(2, 3, 5)
//    val extraList2 = List(1, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 49, 53, 59)
//    val resultList1 = for {
//      w <- (0 to n/60).toList
//      x <- extraList2
//    } yield w*60 + x
    val resultList2 = check1(n) ++ check2(n) ++ check3(n)
    (extraList1 ++ check4(n, resultList2)).sortWith((x, y) => x < y)
  }

  def check1(n: Int): List[Int] = {
    val remList = List(1, 13, 17, 29, 37, 41, 49, 53)
    val resultList = (for {
      x <- (1 to Math.sqrt(n/4).toInt).toList
      y <- (1 to n by 2).toList
    } yield 4*x*x + y*y).filter(_ <= n)
    resultList.filter(x => remList.contains(x%60))
  }

  def check2(n: Int): List[Int] = {
    val remList = List(7, 19, 31, 43)
    val resultList = (for {
      x <- (1 to Math.sqrt(n/3).toInt by 2).toList
      y <- (2 to n by 2).toList
    } yield 3*x*x + y*y).filter(_ <= n)
    resultList.filter(x => remList.contains(x%60))
  }

  def check3(n: Int): List[Int] = {
    val remList = List(11, 23, 47, 59)
    val resultList = (for {
      x <- (2 to Math.sqrt(n/3).toInt).toList
      y <- (x-1 to 1 by 2).toList
    } yield 3*x*x - y*y).filter(_ <= n)
    resultList.filter(x => remList.contains(x%60))
  }

  def check4(n: Int, startList: List[Int]): List[Int] = {
    val remList = List(1, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 49, 53, 59)
    val resultList = (for {
      x <- (0 to Math.sqrt(n/60).toInt).toList
      y <- remList
    } yield 60*x + y).filter(_ >= 7)
    val middleList = startList.filter(x => resultList.contains(x))
    val removeList = for {
      m <- middleList
      x <- (0 to n/60).toList
      y <- remList
    } yield m*m * (x*60 + y)
    startList.filter(x => !removeList.contains(x))
  }

  println(atkinSieve(200))

}
