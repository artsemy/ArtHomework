package implicits

object Less7 {

  object MoreImplicitParameters {
    trait Show[-T] {
      def apply(value: T): String
    }

    def show[T: Show](value: T): String =
      implicitly[Show[T]].apply(value)

    object syntax {
      implicit class ShowOps[T: Show](inner: T) {
        def show: String = MoreImplicitParameters.show(inner)
      }
    }

    object instances {
      implicit val stringShow: Show[String] = (value: String) => value
      implicit val intShow: Show[Int] = (value: Int) => value.toString
      implicit def seqShow[T: Show]: Show[Seq[T]] =
        (value: Seq[T]) => value.map(show(_)).mkString("(", ", ", ")")
    }
  }

  /*
  Exercise 2.

  Let us create a reverseShow method which should be defined for any T which has a Show type-class instance
   */
  object Exercise2 {
    import MoreImplicitParameters.Show

    def reverseShow[T](value: T)(implicit show: Show[T]): String = {
      show.apply(value).reverse
    }

    object syntax {
      implicit class Name[T: Show](inner: T) {
        def reverseShow: String = Exercise2.reverseShow(inner)
      }
    }
  }

  /*
  Exercise 3.

  There are some type-classes in Scala standard library.

  Let's get to know them better!
   */
  object Exercise3 {
    case class HDEYears(value: Long)

    object instances {
      implicit val HDEYearsOrdering: Ordering[HDEYears] = (x: HDEYears, y: HDEYears) => (x.value - y.value).toInt
    }

    /*
    should be defined on any T which has Ordering[T] and return second biggest value from the sequence
    if it exists

    should work on our custom HDEYears

    change the signature accordingly, add implicit instances if needed
     */
    def secondBiggestValue[T: Ordering](values: Seq[T]): Option[T] = {
      values.sorted match {
        case Nil => None
        case _ :: Nil => None
        case twoAndMore => Some(twoAndMore.tail.head)
      }
    }

    //____________________________________________________________________________________________________________
    case class CustomNumber(value: Float)

    trait Summable[T] {
      def count(seq: Seq[T]): Option[T]
    }

    object instances2 {
      implicit val customNumberSummable: Summable[CustomNumber] = (seq: Seq[CustomNumber]) => {
        if (seq.isEmpty) None
        else Some(seq.foldLeft(CustomNumber(0))((x, y) => CustomNumber(x.value + y.value)))
      }
    }

    object syntax2 {
      implicit class Custom[T: Summable](seq: Seq[T]) {
        def sum2: Option[T] = Exercise3.sum2(seq)
      }
    }

    /*
    should be defined on any T which has Summable[T], should return sum value if it can be obtained

    should work on our custom CustomNumber

    change the signature accordingly, add implicit instances if needed
     */
    def sum2[T](values: Seq[T])(implicit summable: Summable[T]): Option[T] = summable.count(values)
  }

  def main(args: Array[String]): Unit = {
    //ex2
    import Exercise2.syntax._
    import MoreImplicitParameters.instances._
    println(12346789.reverseShow)
    //ex3 p1
    import Exercise3._
    import Exercise3.instances._
    val seq = List(HDEYears(20), HDEYears(10), HDEYears(15))
    println(secondBiggestValue(seq))
    //ex3 p2
    import Exercise3.instances2._
    import Exercise3.syntax2._
    val seq2 = List(CustomNumber(2.3f), CustomNumber(5.5f), CustomNumber(2.1f))
    println(seq2.sum2) //sum2 because `sum` asks Numerics[T]; or not to use syntax_
  }

}
