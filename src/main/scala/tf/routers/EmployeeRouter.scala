package tf.routers

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.syntax.all.*
import tf.domain.employee.Employee
import tf.services.EmployeeService
import tf.validation.EmployeeValidator

import java.util.UUID

object EmployeeRouter {

  def apply[F[_]: Monad](employeeService: EmployeeService[F]): Kleisli[OptionT[F, *], List[String], String] =
    Kleisli[OptionT[F, *], List[String], String] {
      case "all" :: _ =>
        OptionT.liftF {
          for {
            all <- employeeService.all
          } yield all.toString()
        }

      case "create" :: birthday :: firstName :: lastName :: salary :: position :: _ =>
        OptionT.liftF {
          for {
//        employeeId <- UUID.randomUUID() // fix
            result <- employeeService.create(birthday, firstName, lastName, salary, position)
          } yield result.toString
        }
      case "update" :: employeeId :: birthday :: firstName :: lastName :: salary :: position :: _ =>
      OptionT.liftF {
        for {
        validEmployee <- EmployeeValidator.validate(birthday, firstName, lastName, salary, position)

        }
      }
    }
}
