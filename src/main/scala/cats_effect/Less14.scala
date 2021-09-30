package cats_effect

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

object Less14 {

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    for {
      _   <- ioa
      res <- iob
    } yield res
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    for {
      res <- ioa
      _   <- iob
    } yield res
  }

  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] = {
    io.flatMap(_ => forever(io))
  }

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.map(_ => value)
  }

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = {
    ioa.map(_ => ())
  }

  /** Exercises:
    *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
    *    - return the result in an IO
    *    - if errored or cancelled, return a failed IO
    *
    *  2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
    *    - if both IOs complete successfully, tuple their results
    *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
    *    - if the first IO doesn't error but second IO returns an error, raise that error
    *    - if one (or both) canceled, raise a RuntimeException
    *
    *  3. Write a function that adds a timeout to an IO:
    *    - IO runs on a fiber
    *    - if the timeout duration passes, then the fiber is canceled
    *    - the method returns an IO[A] which contains
    *      - the original value if the computation is successful before the timeout signal
    *      - the exception if the computation is failed before the timeout signal
    *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
    */
  // 1

  def processResultsFromFiber[A](io: IO[A])(implicit cs: ContextShift[IO]): IO[A] = {
    ???
  }

  def testEx1() = ???

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B])(implicit cs: ContextShift[IO]): IO[(A, B)] = {
    ???
  }

  def testEx2() = ???

  // 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = ???

  def testEx3() = ???

}
