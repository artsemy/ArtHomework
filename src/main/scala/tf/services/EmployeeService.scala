package tf.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import tf.domain.employee.Employee
import tf.validation.EmployeeValidator
import tf.validation.EmployeeValidator._

trait EmployeeService[F[_]] {

  def all: F[List[Employee]]

  def create(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[Either[EmployeeValidationError, Employee]]

  def update(
    id:        String,
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[Either[EmployeeValidationError, Boolean]]

  def find(id: String): F[Option[Employee]]

  def delete(id: String): F[Boolean]
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
    EmployeeValidator
      .validate(birthday = birthday, firstName = firstName, lastname = lastName, salary = salary, position = position)
      .traverse { case e @ Employee(_, _, _, _, _, _) =>
        for {
          _ <- empList.update(e :: _)
        } yield e
      }
  }

  override def update(
    id:        String,
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[Either[EmployeeValidationError, Boolean]] = {
    EmployeeValidator
      .validate(id, birthday, firstName, lastName, salary, position)
      .traverse { employeeUpdate =>
        empList.modify { list =>
          if (!list.forall(e => e.employeeId != employeeUpdate.employeeId))
            list.filterNot(_.employeeId == employeeUpdate.employeeId).appended(employeeUpdate) -> true
          else list                                                                            -> false
        }
      }
  }

  override def find(idS: String): F[Option[Employee]] = {
    val res = validateId(idS).traverse { id =>
      empList.get.map(x => x.find(e => e.employeeId.value == id))
    }
    res.map(x => x.getOrElse(None))
  }

  override def delete(idS: String): F[Boolean] = {
    val res = validateId(idS).traverse { id =>
      empList.modify(list =>
        list.filterNot(e => e.employeeId.value == id) -> list.forall(e => e.employeeId.value == id) //fix
      )
    }
    res.map(x => x.getOrElse(false))
  }

}
