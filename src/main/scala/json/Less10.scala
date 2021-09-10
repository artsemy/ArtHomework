package json

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import Models._

object Less10 {

  def main(args: Array[String]): Unit = {
    def v1 = "https://jsonplaceholder.typicode.com/posts"
    def v2 = "https://jsonplaceholder.typicode.com/comments"
    def v3 = "https://jsonplaceholder.typicode.com/albums"
    def v4 = "https://jsonplaceholder.typicode.com/todos"
    def v5 = "https://jsonplaceholder.typicode.com/users"
    printSimpleArr[Post](v1)
    println("-" * 100)
    printSimpleArr[Comment](v2)
    println("-" * 100)
    printSimpleArr[Album](v3)
    println("-" * 100)
    printSimpleArr[Todo](v4)
    println("-" * 100)
    printSimpleArr[User](v5)
    println("-" * 100)
  }

  def printSimpleArr[T](url: String)(implicit encodeT: Decoder[T]): Unit = {
    val arrSource = scala.io.Source.fromURL(url)
    val arrString = arrSource.mkString
    val result: Either[Error, List[T]] = decode[List[T]](arrString)
    arrSource.close()
    result match {
      case Left(_)      => println("error")
      case Right(value) => println(value.take(3).mkString("\n\n"))
    }
  }

}
