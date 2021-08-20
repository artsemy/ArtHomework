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

  object part3 {
    // Task 2

    // Implement a special sort which sorts the keys of a map (K) according to their associated
    // values (V).
    //
    // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
    //
    // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
    // values.
    //
    // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
    // with values from K, and the associated value V for these values in the `Set`.
    //
    // For example:
    //
    // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
    // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
    def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
      val l1 = map.toList

    }
  }

  def main(args: Array[String]): Unit = {
//    test1()
//    test2(limit = 7, len = 4)
    test3()
  }

  def test1(): Unit = {
    println(part1.totalVegetableWeights)
  }

  def test2(start: Int = 1, limit: Int, len: Int): Unit = {
    val startSet = (start to limit).toSet
    val resultSet = part2.allSubsetsOfSizeN(startSet, len)
    val validSet = startSet.subsets(len).toSet
    println(resultSet == validSet)
  }

  def test3(): Unit = {
    val initMap = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val resultList = part3.sortConsideringEqualValues(initMap)
    val validList = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    println(resultList == validList)
  }


}
