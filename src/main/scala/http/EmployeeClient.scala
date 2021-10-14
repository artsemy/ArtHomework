package http

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.*
import org.http4s.implicits.*
import tf.domain.employee.{CreateEmployeeDTO, EmployeeDTO}
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io.*

import scala.concurrent.ExecutionContext

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
