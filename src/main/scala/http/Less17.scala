package http

import cats.data.Validated
import http.Game.{InitParams, InitParamsDTO}
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.circe.CirceEntityCodec._

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

object Game {
  final case class InitParamsDTO(min: String, max: String, attempts: String)
  final case class InitParams(min: Int, max: Int, attempts: Int)
}

object GuessServer extends IOApp {

  private val number: AtomicInteger = new AtomicInteger(0)
  private val attemptNumber = new AtomicInteger(0)
  final val LOWER           = "lover"
  final val GREATER         = "greater"
  final val EQUAL           = "equal"
  final val NO_ATTEMPTS     = "no attempts left"
  final val BAD_INPUT       = "bad input"
  final val GAME_STARTED    = "game started"

  val logger: SelfAwareStructuredLogger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  def validateInitParams(initParamsDTO: InitParamsDTO): Option[InitParams] = for {
    min      <- initParamsDTO.min.toIntOption
    max      <- initParamsDTO.max.toIntOption
    attempts <- initParamsDTO.attempts.toIntOption
  } yield InitParams(min, max, attempts)

  def generateNumber(params: InitParamsDTO): String = {
    validateInitParams(params) match {
      case None => BAD_INPUT
      case Some(p) =>
        val randomNumber = Random.nextInt(p.max - p.min) + p.min + 1
        number.set(randomNumber)
        attemptNumber.set(p.attempts)
        GAME_STARTED
    }
  }

  def makeAttempt(guessValue: Int): String = {
    val generatedValue = number.get()
    val attemptsLeft   = attemptNumber.getAndUpdate(x => x - 1)
    val compareResult =
      if (guessValue > generatedValue) LOWER
      else if (guessValue < generatedValue) GREATER
      else EQUAL
    if (attemptsLeft == 1 && compareResult != EQUAL) NO_ATTEMPTS
    else compareResult
  }

  private val gameRouters = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    implicit val localGuessDecoder: QueryParamDecoder[Int] = { guessValue =>
      Validated
        .catchNonFatal(guessValue.value.toInt)
        .leftMap(t => ParseFailure(s"Failed parsing ${guessValue.value} to Int", t.getMessage))
        .toValidatedNel
    }
    object GuessMatcher extends QueryParamDecoderMatcher[Int](name = "guessValue")

    HttpRoutes.of[IO] {

      // curl -XPOST "localhost:9001/game/start" -d '{"min": "1", "max": "10", "attempts": "5"}' -H "Content-Type: application/json"
      case req @ POST -> Root / "game" / "start" =>
        req.as[InitParamsDTO].flatMap { params =>
          val result = generateNumber(params)
          Ok(result)
        }

      // curl -XPOST "localhost:9001/game/guess?guessValue=3"
      case POST -> Root / "game" / "guess" :? GuessMatcher(guessValue) =>
        val result = makeAttempt(guessValue)
        Ok(result)
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
    _     <- printLine("attempt")
    middle = (max - min) / 2 + min
    _     <- printLine(s"$middle")
    response <- {

      client.expect[String](Method.POST((uri / "game" / "guess").withQueryParam("guessValue", middle.toString)))
    }
    res <-
      response match {
        case LOWER        => findNumber(client, min, middle)
        case GREATER      => findNumber(client, middle, max)
        case EQUAL        => s"win $middle".pure[IO]
        case errorMessage => errorMessage.pure[IO]
      }
  } yield res

  override def run(args: List[String]): IO[ExitCode] = {
    val min           = 0
    val max           = 100
    val attemptNumber = 5

    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .use { client =>
        for {
          start <- {
            import io.circe.generic.auto._
            import org.http4s.circe.CirceEntityCodec._
            client.expect[String](
              Method.POST(InitParamsDTO(min.toString, max.toString, attemptNumber.toString), uri / "game" / "start")
            )
          }
          _   <- printLine(start)
          res <- findNumber(client, min, max)
          _   <- printLine(res)
        } yield ()
      }
      .as(ExitCode.Success)
  }

}
