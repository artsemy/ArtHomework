package http

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import tf.services.EmployeeService

import scala.concurrent.ExecutionContext

object EmployeeServer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = (for {
    service <- EmployeeService.of[IO]
    rout     = HttpRouter.routes(service)
    res <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp({ rout }.orNotFound)
      .serve
      .compile
      .drain
  } yield res).as(ExitCode.Success)

  // curl "localhost:9001/employee/all"
  // curl "localhost:9001/employee/find/dbd612dc-b1b0-46fb-864b-cdad53bf95d1"
  // curl -XPOST "localhost:9001/employee/delete/dbd612dc-b1b0-46fb-864b-cdad53bf95d1"
  // curl -XPOST "localhost:9001/employee/create" -H "Content-Type: application/json" -d "{\"birthday\": \"2010-10-10T11:11:00Z\", \"firstName\": \"Arty\", \"lastName\": \"Arty\", \"salary\": \"500.00USD\", \"position\": \"junior\"}"
  // curl -XPOST "localhost:9001/employee/update" -H "Content-Type: application/json" -d "{\"employeeId\": \"9a1618a7-3e4b-4ec6-b565-be684b34ed38\", \"birthday\": \"2010-10-10T11:11:00Z\", \"firstName\": \"Marty\", \"lastName\": \"Marty\", \"salary\": \"500.00USD\", \"position\": \"junior\"}"

}
