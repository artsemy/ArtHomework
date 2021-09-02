package typeclass

object Less8 {

  trait HashCode[A] {
    def hash(x: A): Int
  }

  object HashCode {
    def apply[A](implicit instance: HashCode[A]): HashCode[A] = instance
  }

  object HashCodeSyntax {
    implicit class HashCodeOps[A](x: A) {
      def hash(implicit i: HashCode[A]): Int = i.hash(x)
    }
  }

  object HashCodeInstances {
    implicit val StringHashCode: HashCode[String] = (x: String) => x.hashCode
  }

  def hash[A: HashCode](x: A): Int = HashCode[A].hash(x)

  def main(args: Array[String]): Unit = {
    import HashCodeSyntax._
    import HashCodeInstances._
    val str = "abc"
    println(str.hashCode == str.hash && str.hashCode == hash(str))
  }

}
