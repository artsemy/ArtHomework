package http

import cats.MonadError
import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import io.circe.Json
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.circe.CirceEntityCodec.*
import tf.routers.EmployeeRouter
import tf.services.EmployeeService

import scala.concurrent.ExecutionContext

object EmployeeServer extends IOApp {

  private val router = for {
    service <- EmployeeService.of[IO]
    rr       = EmployeeRouter(service)
  } yield rr

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  private def count(list: List[String]): IO[Option[String]] = {
    val resp = for {
      rr  <- router
      res <- rr(list).value
    } yield res
    resp
  }

  private val employeeRouters = {

    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO] {

      // curl "localhost:9001/employee/all"
      case GET -> Root / "employee" / "all" =>
        val res = count(List("all"))
        Ok(res)

      // curl "localhost:9001/employee/find/dbd612dc-b1b0-46fb-864b-cdad53bf95d1"
      case GET -> Root / "employee" / "find" / id =>
        val res = count(List("find", id))
        Ok(res)

      // curl -XPOST "localhost:9001/employee/delete/dbd612dc-b1b0-46fb-864b-cdad53bf95d1"
      case POST -> Root / "employee" / "delete" / id =>
        val res = count(List("delete", id))
        Ok(res)

      // curl -XPOST "localhost:9001/employee/create" -H "Content-Type: application/json" -d "{\"list\":[\"2010-10-10T11:11:00Z\", \"Arty\", \"Arty\", \"500.00USD\", \"junior\"]}"
      case req @ POST -> Root / "employee" / "create" =>
        req.as[List[String]].flatMap { list =>
          val res = count("create" :: list)
          Ok(res)
        }

      // curl -XPOST "localhost:9001/employee/update" -H "Content-Type: application/json" -d "{\"list\":[\"dbd612dc-b1b0-46fb-864b-cdad53bf95d1\", \"2010-10-10T11:11:00Z\", \"Marty\", \"Marty\", \"500.00USD\", \"junior\"]}"
      case req @ POST -> Root / "employee" / "delete" =>
        req.as[List[String]].flatMap { list =>
          val res = count("delete" :: list)
          Ok(res)
        }
    }

  }

  private[http] val httpApp = {
    employeeRouters
  }.orNotFound

}
