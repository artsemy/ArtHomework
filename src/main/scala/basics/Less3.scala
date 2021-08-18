package basics

object Less3 {

  object part1 {
    val vegetableAmounts = Map(
      "tomatoes"  -> 17,
      "peppers"   -> 234,
      "olives"    -> 32,
      "cucumbers" -> 323,
    )

    val vegetableWeights = Map(
      ("pumpkins", 10),
      ("cucumbers", 20),
      ("olives", 2),
    )

    // Exercise 3. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
    // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
    //
    // For example, the total weight of "olives" is 2 * 32 == 64.
    val totalVegetableWeights: Map[String, Int] = {
      vegetableAmounts.map {case (key, amount) => (key, amount * vegetableWeights.getOrElse(key, -1))}
    }
  }

  object part2 {
    // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
    // Hints for implementation:
    //   - Handle the trivial case where `n == 1`.
    //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
    //     that don't include `elem`, and add `elem` to them.
    def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
      n match {
        case 1 => Set(set)
        case _ => f1(set, n)
      }

      def f1(set: Set[A], n: Int): Set[Set[A]] = {
        if (set.size > n) g1(set, n-1)
        else Set(set)
      }

      def g1(set: Set[A], n: Int): Set[Set[A]] = {
        val h1 = set.head
        val t1 = set.tail
        val set2 = if (t1.size >= n) {
          println("h=" + h1 + " t=" + t1.toString() + " n=" + n)
          g1(t1, n)
        }
        if (n > 1 && t1.size >= n) {
          println()
          g1(t1, n-1)
        }
        Set(set)
      }

      println(n)
      Set(set)
    }
  }

  def main(args: Array[String]): Unit = {
//    println(part1.totalVegetableWeights)
    println(part2.allSubsetsOfSizeN(Set(1,2,3,4,0), 3))
  }

}
