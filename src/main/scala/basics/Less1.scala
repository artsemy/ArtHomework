package basics

object Less1 extends App {

  def fibonacci(n: Int): Either[String, Int] = {
    n match {
      case x if x < 0 => Left("Error message")
      case 0 => Right(0)
      case 1 => Right(1)
      case y if y > 1 => if (fibonacci(y-1).isRight && fibonacci(y-2).isRight) {
        Right(fibonacci(y-1).getOrElse(-1) + fibonacci(y-2).getOrElse(-1))
      } else Left("Super magic") // fix
      case _ => Left("Magic here")
    }
  }

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
    list.filter(x => !list.exists(y => list.exists(z => x == y * z))) //???
  }

  println(atkinSieve(200))
  // 91 133 143
  // 91 - 7 13
  // 133 - 7 19
  // 143 - 11 13
}
