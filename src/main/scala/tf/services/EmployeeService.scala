package tf.services

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits.*
import tf.domain.employee.{Employee, EmployeeDTO}
import tf.validation.EmployeeValidator
import tf.validation.EmployeeValidator.*

import java.util.UUID

trait EmployeeService[F[_]] {

  def all: F[List[Employee]]

  def create(employeeDTO: EmployeeDTO): F[Either[EmployeeValidationError, Employee]]

  def update(employeeDTO: EmployeeDTO): F[Either[EmployeeValidationError, Boolean]]

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

  override def create(employeeDTO: EmployeeDTO): F[Either[EmployeeValidationError, Employee]] = {
    val empWithId = employeeDTO.copy(employeeId = UUID.randomUUID().toString)
    EmployeeValidator
      .validate(empWithId)
      .traverse { case e @ Employee(_, _, _, _, _, _) =>
        for {
          _ <- empList.update(e :: _)
        } yield e
      }
  }

  override def update(employeeDTO: EmployeeDTO): F[Either[EmployeeValidationError, Boolean]] = {
    EmployeeValidator
      .validate(employeeDTO)
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
      empList.get.map(x => x.find(e => e.employeeId.value == id.value))
    }
    res.map(x => x.getOrElse(None))
  }

  override def delete(idS: String): F[Boolean] = {
    val res = validateId(idS).traverse { id =>
      empList.modify(list =>
        list.filterNot(e => e.employeeId.value == id.value) -> list.exists(e => e.employeeId == id)
      )
    }
    res.map(x => x.getOrElse(false))
  }

}
