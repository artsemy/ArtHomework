package http

import cats.data.Validated
import http.Game.{Guess, GuessDTO, InitParams}
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import doobie.`enum`.JdbcType.Integer
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.server.blaze.BlazeServerBuilder

import java.time.LocalDate
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
  final case class InitParams(min: String, max: String, attempts: String)
  final case class GuessDTO(guessValue: String)
  final case class Guess(guessValue: Int)
}

object GuessServer extends IOApp {

  private val number: AtomicInteger = new AtomicInteger(0)
  private val attemptNumber = new AtomicInteger(0)
  final val LOWER           = "lover"
  final val GREATER         = "greater"
  final val EQUAL           = "equal"
  final val NO_ATTEMPTS     = "no attempts left"

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
    attempt      = attemptNumber.getAndUpdate(x => x - 1)
    compareRes =
      if (guessNumber > num) LOWER
      else if (guessNumber < num) GREATER
      else EQUAL
    answer =
      if (attempt == 1 && compareRes != EQUAL) NO_ATTEMPTS
      else compareRes
  } yield answer

  private val gameRouters = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    implicit val localGuessDecoder: QueryParamDecoder[Guess] = { guessValue =>
      Validated
        .catchNonFatal(Guess(guessValue.value.toInt))
        .leftMap(t => ParseFailure(s"Failed parsing ${guessValue.value} to Int", t.getMessage))
        .toValidatedNel
    }
    object GuessMatcher extends QueryParamDecoderMatcher[Guess](name = "guessValue")

    HttpRoutes.of[IO] {
//      // curl "localhost:9001/game/start/1/5/3"
//      case GET -> Root / "game" / "start" / minValue / maxValue / attemptNum =>
//        parseAndGenerate(minValue, maxValue, attemptNum) match {
//          case Some(value) =>
//            logger.info(s"generated: $value")
//            Ok(s"game started")
//          case None => BadRequest("invalid max, min or attempt values")
//        }
//      // curl "localhost:9001/game/guess/2"
//      case GET -> Root / "game" / "guess" / guessValue =>
//        parseAndCompare(guessValue) match {
//          case Some(value) => Ok(value)
//          case None        => BadRequest("invalid guess value")
//        }

      // curl -XPOST "localhost:9001/game/start" -d '{"min": "1", "max": "10", "attempts": "5"}' -H "Content-Type: application/json"
      case req @ POST -> Root / "game" / "start" =>
        req.as[InitParams].flatMap { params =>
          Ok(s"game started: min - ${params.min}, max - ${params.max}, attempts - ${params.attempts}")
        }

      // curl -XPOST "localhost:9001/game/guess?guessValue=3"
      case POST -> Root / "game" / "guess" :? GuessMatcher(guessValue) =>
        Ok(attemptNumber.get())
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
      implicit val Encoder: EntityEncoder[IO, GuessDTO] =
        EntityEncoder.stringEncoder[IO].contramap { guess: GuessDTO =>
          s"(${guess.guessValue})"
        }
      client.expect[String](Method.POST(GuessDTO(middle.toString), uri / "game" / "guess"))
    }
//    response <- client.expect[String](uri / "game" / "guess" / middle.toString)
    res <- response match {
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
          res <- {
            import io.circe.generic.auto._
            import org.http4s.circe.CirceEntityCodec._
            client.expect[String](
              Method.POST(InitParams(min.toString, max.toString, attemptNumber.toString), uri / "game" / "start")
            )
          }
          res <- findNumber(client, min, max)
          _   <- printLine(res)
        } yield ()
      }
      .as(ExitCode.Success)
  }

}
