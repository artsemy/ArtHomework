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
      g2(Set.empty, set, n)
    }

    def g2[A](leftSet: Set[A], rightSet: Set[A], n: Int): Set[Set[A]] = {
      val s1 = if (rightSet.size > n && n > 1) {
        g2(Set(rightSet.head), rightSet.tail, n-1)
      } else Set.empty
      val s2 = if (rightSet.size > n && n > 1) {
        g2(Set(rightSet.tail.head), rightSet.tail.tail, n-1)
      } else Set.empty
      val s3 = if (n == 1) rightSet.map(x => leftSet + x)
      else (s1 ++ s2).map(x => leftSet ++ x)
      s3
    }
  }

  def main(args: Array[String]): Unit = {
//    println(part1.totalVegetableWeights)
    println(part2.allSubsetsOfSizeN(Set(1,2,3,4,0), 3))
  }

}
