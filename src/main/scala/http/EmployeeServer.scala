package http

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.JsonCodec
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.server.blaze.BlazeServerBuilder
import tf.routers.EmployeeRouter
import tf.services.EmployeeService
import tf.domain.employee.EmployeeDTO
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext

@JsonCodec
final case class CreateEmployeeDTO(
  birthday:  String,
  firstName: String,
  lastName:  String,
  salary:    String,
  position:  String
)

object EmployeeServer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  private val router = new AtomicReference(for {
    service <- EmployeeService.of[IO]
    rr       = EmployeeRouter(service)
  } yield rr)

  private def count(list: List[String]): IO[Option[String]] = {
    val resp = for {
      rr  <- router.get()
      res <- rr(list).value
    } yield res
    resp
  }

  private val employeeRouters = {

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

      // curl -XPOST "localhost:9001/employee/create" -H "Content-Type: application/json" -d "{\"birthday\": \"2010-10-10T11:11:00Z\", \"firstName\": \"Arty\", \"lastName\": \"Arty\", \"salary\": \"500.00USD\", \"position\": \"junior\"}"
      case req @ POST -> Root / "employee" / "create" =>
        req.as[CreateEmployeeDTO].flatMap { dto =>
          val res = count(List("create", dto.birthday, dto.firstName, dto.lastName, dto.salary, dto.position))
          Ok(res)
        }

      // curl -XPOST "localhost:9001/employee/update" -H "Content-Type: application/json" -d "{\"employeeId\": \"9a1618a7-3e4b-4ec6-b565-be684b34ed38\", \"birthday\": \"2010-10-10T11:11:00Z\", \"firstName\": \"Marty\", \"lastName\": \"Marty\", \"salary\": \"500.00USD\", \"position\": \"junior\"}"
      case req @ POST -> Root / "employee" / "update" =>
        req.as[EmployeeDTO].flatMap { dto =>
          val res =
            count(List("update", dto.employeeId, dto.birthday, dto.firstName, dto.lastName, dto.salary, dto.position))
          Ok(res)
        }
    }

  }

  private[http] val httpApp = {
    employeeRouters
  }.orNotFound

}

object EmployeeClient extends IOApp {

  private val uri = uri"http://localhost:9001"
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .use { client =>
        for {
          empty    <- client.expect[String](uri / "employee" / "all")
          _        <- printLine("empty: " + empty)
          createEmp = CreateEmployeeDTO("2010-10-10T11:11:00Z", "Arty", "Arty", "500.00USD", "junior")
          created  <- client.expect[String](Method.POST(createEmp, uri / "employee" / "create"))
          _        <- printLine("created: " + created)

          nonEmpty <- client.expect[String](uri / "employee" / "all")
          _        <- printLine("nonEmpty: " + nonEmpty)
          //for test
          employeeId = created.substring(26, 62)
//          _         <- printLine("employeeId: " + employeeId)
          updateEmp = EmployeeDTO(
            employeeId,
            "2010-10-10T11:11:00Z",
            "Marty",
            "Marty",
            "500.00USD",
            "junior"
          )
          updated <- client.expect[String](Method.POST(updateEmp, uri / "employee" / "update"))
          _       <- printLine("updated: " + updated)
          found   <- client.expect[String](uri / "employee" / "find" / employeeId)
          _       <- printLine("found: " + found)
          deleted <- client.expect[String](Method.POST(uri / "employee" / "delete" / employeeId))
          _       <- printLine("deleted: " + deleted)
          empty2  <- client.expect[String](uri / "employee" / "all")
          _       <- printLine("empty2: " + empty2)
        } yield ()
      }
      .as(ExitCode.Success)
  }

}
