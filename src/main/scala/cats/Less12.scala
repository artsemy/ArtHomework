package cats

import scala.annotation.tailrec

object Less12 {

  trait CustomMonad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }

  /** Ex 5.1 implement CustomMonad for List
    */
  val listM: CustomMonad[List] = new CustomMonad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
      fa match {
        case ::(head, next) => f(head) ::: flatMap(next)(f)
        case Nil            => Nil
      }
    }
  }

  /** Ex 5.2 implement CustomMonad for Option
    */
  val optionM: CustomMonad[Option] = new CustomMonad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case Some(value) => f(value)
        case None        => None
      }
  }

  /** Ex 7.0 implement traverse function for Option
    */
  def optionTraverse[A](input: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def run[B](initList: List[Option[B]], valueList: List[B]): Option[List[B]] = {
      initList match {
        case ::(head, next) =>
          head match {
            case None        => None
            case Some(value) => run(next, valueList :+ value)
          }
        case Nil => Some(valueList)
      }
    }

    run(input, List.empty)
  }

  def optionTraverse2[A](input: List[Option[A]]): Option[List[A]] = {
    val res = for {
      opElem  <- input
      elemVal <- opElem
    } yield elemVal
    if (input.length == res.length) Some(res) else None
  }

  /** Ex 7.1 implement traverse for Either. Use fail fast approach (the first error encountered is returned.)
    */
  def eitherTraverse[E, A](input: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def run[B](initList: List[Either[E, B]], valueList: List[B]): Either[E, List[B]] = {
      initList match {
        case Nil => Right(valueList)
        case ::(head, next) =>
          head match {
            case Left(value)  => Left(value)
            case Right(value) => run(next, valueList :+ value)
          }
      }
    }

    run(input, List.empty)
  }

  trait CustomError
  object CustomError {
    final case object TypeOneError extends CustomError
    final case object TypeTwoError extends CustomError
    final case object TypeThreeError extends CustomError
    final case object TypeFourError extends CustomError
  }

}
