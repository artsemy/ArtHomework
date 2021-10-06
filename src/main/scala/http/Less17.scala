package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.util.Random

object Less17 {}

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.

object GuessServer extends IOApp {

  private val number: AtomicInteger = new AtomicInteger(0)
  private val attemptNumber = new AtomicInteger(0)
  final val LOWER           = "lover"
  final val GREATER         = "greater"
  final val EQUAL           = "equal"
  final val NO_ATTEMPTS     = "no attempts left"

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  def parseAndGenerate(minValue: String, maxValue: String, attemptNum: String): Option[Int] = for {
    parsedMin     <- minValue.toIntOption
    parsedMax     <- maxValue.toIntOption
    parsedAttempt <- attemptNum.toIntOption
    randomNumber   = Random.nextInt(parsedMax - parsedMin) + parsedMin + 1
    _              = number.set(randomNumber)
    _              = attemptNumber.set(parsedAttempt)
  } yield randomNumber

  def parseAndCompare(guessValue: String): Option[String] = for {
    guessNumber <- guessValue.toIntOption
    num          = number.get()
    attempt      = attemptNumber.get()
    answer = attempt match {
      case 0 => NO_ATTEMPTS
      case _ =>
        attemptNumber.set(attempt - 1)
        if (guessNumber > num) LOWER
        else if (guessNumber < num) GREATER
        else EQUAL
    }
  } yield answer

  private val gameRouters = HttpRoutes.of[IO] {

    // curl "localhost:9001/game/start/1/5/3"
    case GET -> Root / "game" / "start" / minValue / maxValue / attemptNum =>
      parseAndGenerate(minValue, maxValue, attemptNum) match {
        case Some(value) => Ok(s"game started $value")
        case None        => BadRequest("invalid max or min values")
      }
    // curl "localhost:9001/game/guess/2"
    case GET -> Root / "game" / "guess" / guessValue =>
      parseAndCompare(guessValue) match {
        case Some(value) => Ok(value)
        case None        => BadRequest("invalid guess value")
      }

  }

  private[http] val httpApp = {
    gameRouters
  }.orNotFound

}

import GuessServer.{EQUAL, GREATER, LOWER}
object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def findNumber(client: Client[IO], min: Int, max: Int): IO[String] = for {
    _        <- printLine("attempt")
    middle    = (max - min) / 2 + min
    _        <- printLine(s"$middle")
    response <- client.expect[String](uri / "game" / "guess" / middle.toString)
    res <- response match {
      case LOWER        => findNumber(client, min, middle)
      case GREATER      => findNumber(client, middle, max)
      case EQUAL        => s"win $middle".pure[IO]
      case errorMessage => errorMessage.pure[IO]
    }
    _ <- Thread.sleep(1000).pure[IO]
  } yield res

  override def run(args: List[String]): IO[ExitCode] = {
    val min           = 0
    val max           = 100
    val attemptNumber = 5
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use { case (client, _) =>
        for {
          test <- client.expect[String](uri / "game" / "start" / min.toString / max.toString / attemptNumber.toString)
          _    <- printLine(test) //testing
          res  <- findNumber(client, min, max)
          _    <- printLine(res)
        } yield ()
      }
      .as(ExitCode.Success)
  }

}
