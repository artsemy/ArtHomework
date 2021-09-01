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
      implicit val HDEYearsOrdering: Ordering[HDEYears] = (x: HDEYears, y: HDEYears) => x.value - y.value match {
        case z if z > 0 => 1
        case z if z < 0 => -1
        case _ => 0
      }
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

    object instances2 {
      implicit val customNumberNumeric: Numeric[CustomNumber] = new Numeric[CustomNumber] {
        override def plus(x: CustomNumber, y: CustomNumber): CustomNumber = CustomNumber(Numeric[Float].plus(x.value, y.value))

        override def minus(x: CustomNumber, y: CustomNumber): CustomNumber = CustomNumber(Numeric[Float].minus(x.value, y.value))

        override def times(x: CustomNumber, y: CustomNumber): CustomNumber = CustomNumber(Numeric[Float].times(x.value, y.value))

        override def negate(x: CustomNumber): CustomNumber = CustomNumber(Numeric[Float].negate(x.value))

        override def fromInt(x: Int): CustomNumber = CustomNumber(x.toFloat)

        override def parseString(str: String): Option[CustomNumber] = Numeric[Float].parseString(str).map(CustomNumber)

        override def toInt(x: CustomNumber): Int = Numeric[Float].toInt(x.value)

        override def toLong(x: CustomNumber): Long = Numeric[Float].toLong(x.value)

        override def toFloat(x: CustomNumber): Float = Numeric[Float].toFloat(x.value)

        override def toDouble(x: CustomNumber): Double = Numeric[Float].toDouble(x.value)

        override def compare(x: CustomNumber, y: CustomNumber): Int = Numeric[Float].compare(x.value, y.value)
      }
    }

    /*
    should be defined on any T which has Summable[T], should return sum value if it can be obtained

    should work on our custom CustomNumber

    change the signature accordingly, add implicit instances if needed
     */
    def sum[T: Numeric](values: Seq[T]): Option[T] = {
      values.length match {
        case 0 => None
        case _ => Some(values.sum)
      }
    }
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
    val seq2 = List(CustomNumber(2.3f), CustomNumber(5.5f), CustomNumber(2.1f))
    println(sum(seq2))
  }

}
