package tf.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import tf.domain.employee.Employee
import tf.validation.EmployeeValidator
import tf.validation.EmployeeValidator.EmployeeValidationError

import java.util.UUID

trait EmployeeService[F[_]] {
  def all: F[List[Employee]]
  def create(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[Either[EmployeeValidationError, Employee]]
  def update(employee: Employee): F[Either[EmployeeValidationError, Boolean]]
  def find(id:         UUID):     F[Option[Employee]]
  def delete(id:       UUID):     F[Boolean]
}

object EmployeeService {

  def of[F[_]: Sync]: F[EmployeeService[F]] = for {
    employees <- Ref.of[F, List[Employee]](List.empty)
  } yield new InMemoryEmployeeService(employees)
}

final private class InMemoryEmployeeService[F[_]: Sync](empList: Ref[F, List[Employee]]) extends EmployeeService[F] {
  override def all: F[List[Employee]] = empList.get

  override def create(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[Either[EmployeeValidationError, Employee]] = {
    EmployeeValidator.validate(birthday, firstName, lastName, salary, position).traverse {
      case e @ Employee(_, _, _, _, _, _) =>
        for {
          _ <- empList.update(e :: _)
        } yield e
    }
  }

  override def update(employee: Employee): F[Either[EmployeeValidationError, Boolean]] = {
    EmployeeValidator
      .validate(
        employee.birthday.toString,
        employee.firstName,
        employee.lastName,
        employee.salary.toString,
        employee.position.getClass.getName
      )
      .traverse { _ =>
        empList.modify { list =>
          if (!list.forall(e => e.employeeId != employee.employeeId))
            list.filterNot(_.employeeId == employee.employeeId).appended(employee) -> true
          else list                                                                -> false
        }
      }
  }

  override def find(id: UUID): F[Option[Employee]] = {
    empList.get.map(x => x.find(e => e.employeeId.value == id))
  }

  override def delete(id: UUID): F[Boolean] = {
    empList.modify(list => list.filterNot(e => e.employeeId.value == id) -> list.forall(e => e.employeeId.value != id))
  }
}
