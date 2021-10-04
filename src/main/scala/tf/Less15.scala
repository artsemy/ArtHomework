package tf

import cats.effect.{ExitCode, IO, IOApp}
import tf.routers.EmployeeRouter
import tf.services.EmployeeService

// You need to design and implement service for management of employees.

// Employee must contain information about (birthday, first name, last name, salary, position)

// - Access to the service should be via Console interface
// - Storage: in memory data structures

// Core features:

// - CRUD of employees
// - Employees validation
// - Add tests for core features

object Less15 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      service <- EmployeeService.of[IO]
      router   = EmployeeRouter(service)
      _       <- ConsoleInterface(router).repl
    } yield ExitCode.Success
  }

}
