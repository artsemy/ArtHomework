package basics

object Less3 {

  object ex1 {
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

  object ex2 {
    // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
    // Hints for implementation:
    //   - Handle the trivial case where `n == 1`.
    //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
    //     that don't include `elem`, and add `elem` to them.
    def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {

      def count(leftSet: Set[A], rightSet: Set[A], n: Int): Set[Set[A]] = {
        val rSize = rightSet.size
        (rSize, n) match {
          case (size, n) if size > n && n > 1 =>
            val s1 = count(Set(rightSet.head), rightSet.tail, n - 1)
            val s2 = count(Set.empty, rightSet.tail, n)
            (s1 ++ s2).map(x => x ++ leftSet)
          case (size, n) if size == n => Set(rightSet)
          case (_, n) if n == 1 => rightSet.map(x => leftSet + x)
        }
      }

      if (n < 1) Set(Set.empty)
      else if (n > set.size) Set.empty
      else count(Set.empty, set, n)
    }
  }

  object ex3 {
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

      def getSetByVal(value: Int, initMap: Map[T, Int]): Set[T] = {
        val filteredMap = initMap.filter {case (_, number) => number == value}
        filteredMap.keySet
      }

      val valSet = map.values.toSet
      val resList = valSet.map(x => (getSetByVal(x, map), x)).toList
      resList.sortBy {case (_, value) => value}
    }
  }

  def main(args: Array[String]): Unit = {
    test1()
    test2(limit = 8, len = 5)
    test3()
  }

  def test1(): Unit = {
    println(ex1.totalVegetableWeights)
  }

  def test2(start: Int = 1, limit: Int, len: Int): Unit = {
    val initSet = (start to limit).toSet
    val resultSet = ex2.allSubsetsOfSizeN(initSet, len)
    val validSet = initSet.subsets(len).toSet
    println(resultSet == validSet)
  }

  def test3(): Unit = {
    val initMap = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val resultList = ex3.sortConsideringEqualValues(initMap)
    val validList = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
    println(resultList == validList)
  }


}
