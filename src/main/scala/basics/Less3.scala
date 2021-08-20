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
      val s1 = if (n > 1 && rightSet.size > n) {
        val partSet = g2(Set(rightSet.head), rightSet.tail, n-1)
        partSet.map(x => x ++ leftSet)
      } else Set.empty
      val s2 = if (n == 1) {
        rightSet.map(x => leftSet+x)
      } else Set.empty
      val s3 = if (rightSet.size > n && n > 1) {
        val partSet = g2(Set.empty, rightSet.tail, n)
        partSet.map(x => x ++ leftSet)
      } else Set.empty
      val s4 = if (rightSet.size == n) {
        Set(rightSet)
      } else Set.empty
      s1 ++ s2 ++ s3 ++ s4
    }
  }

  def main(args: Array[String]): Unit = {
//    test1()
    for {
      len <- 1 to 9
    } yield test2(limit = 10, len = len)

  }

  def test1() = {
    println(part1.totalVegetableWeights)
  }

  def test2(start: Int = 1, limit: Int, len: Int) = {
    val startSet = (start to limit).toSet
    val resultSet = part2.allSubsetsOfSizeN(startSet, len)
    val validSet = startSet.subsets(len).toSet
    println(resultSet == validSet)
  }


}
