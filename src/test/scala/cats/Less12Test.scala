package cats

import cats.Less12._
import org.scalatest.freespec.AnyFreeSpec
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import CustomError._

class Less12Test extends AnyFreeSpec {

  "Methods tests" - {

    "optionTraverse" - {
      "optionTraverse: all /None/" in {
        val listOfOptions: List[Option[String]] = List(None, None, None, None)

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse: all /Some/" in {
        val listOfOptions: List[Option[String]] = List(Some("one"), Some("two"), Some("three"), Some("four"))

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse: one /Some/" in {
        val listOfOptions: List[Option[String]] = List(None, Some("one"), None, None)

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse: one /None/" in {
        val listOfOptions: List[Option[String]] = List(Some("one"), Some("two"), None, Some("three"))

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse(listOfOptions)
        assert(expected == actual)
      }
    }

    "optionTraverse2" - {
      "optionTraverse2: all /None/" in {
        val listOfOptions: List[Option[String]] = List(None, None, None, None)

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse2(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse2: all /Some/" in {
        val listOfOptions: List[Option[String]] = List(Some("one"), Some("two"), Some("three"), Some("four"))

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse2(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse2: one /Some/" in {
        val listOfOptions: List[Option[String]] = List(None, Some("one"), None, None)

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse2(listOfOptions)
        assert(expected == actual)
      }
      "optionTraverse2: one /None/" in {
        val listOfOptions: List[Option[String]] = List(Some("one"), Some("two"), None, Some("three"))

        val expected = listOfOptions.traverse(identity)
        val actual   = optionTraverse2(listOfOptions)
        assert(expected == actual)
      }
    }

    "eitherTraverse" - {
      "eitherTraverse: no Errors" in {
        val listOfEither: List[Either[CustomError, String]] =
          List(Right("one"), Right("two"), Right("three"), Right("four"))

        val expected = listOfEither.traverse(identity)
        val actual   = eitherTraverse(listOfEither)
        assert(expected == actual)
      }
      "eitherTraverse: all Errors" in {
        val listOfEither: List[Either[CustomError, String]] = //ThreeError first in list
          List(Left(TypeThreeError), Left(TypeTwoError), Left(TypeOneError), Left(TypeFourError))

        val expected = listOfEither.traverse(identity)
        val actual   = eitherTraverse(listOfEither)
        assert(expected == actual)
      }
      "eitherTraverse: one Error" in {
        val listOfEither: List[Either[CustomError, String]] =
          List(Right("one"), Left(TypeTwoError), Right("three"), Right("four"))

        val expected = listOfEither.traverse(identity)
        val actual   = eitherTraverse(listOfEither)
        assert(expected == actual)
      }
      "eitherTraverse: all Errors except one" in {
        val listOfEither: List[Either[CustomError, String]] =
          List(Left(TypeOneError), Left(TypeTwoError), Right("three"), Left(TypeFourError))

        val expected = listOfEither.traverse(identity)
        val actual   = eitherTraverse(listOfEither)
        assert(expected == actual)
      }
    }

  }
}
