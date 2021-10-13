package tf.routers

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.syntax.all._
import tf.domain.employee.EmployeeDTO
import tf.services.EmployeeService

object EmployeeRouter {

  def apply[F[_]: Monad](employeeService: EmployeeService[F]): Kleisli[OptionT[F, *], List[String], String] =
    Kleisli[OptionT[F, *], List[String], String] {
      case "all" :: _ =>
        OptionT.liftF {
          for {
            all <- employeeService.all
          } yield all.toString()
        }

      // create 2020-10-10T12:12:00Z Arty Arty 100.00USD junior
      case "create" :: birthday :: firstName :: lastName :: salary :: position :: _ =>
        OptionT.liftF {
          val empDto = EmployeeDTO("", birthday, firstName, lastName, salary, position)
          for {
            result <- employeeService.create(empDto)
          } yield result.toString
        }

      case "update" :: employeeId :: birthday :: firstName :: lastName :: salary :: position :: _ =>
        OptionT.liftF {
          val empDto = EmployeeDTO(employeeId, birthday, firstName, lastName, salary, position)
          for {
            result <- employeeService.update(empDto)
          } yield result.toString
        }

      case "find" :: employeeId :: _ =>
        OptionT.liftF {
          for {
            result <- employeeService.find(employeeId)
          } yield result.toString
        }

      case "delete" :: employeeId :: _ =>
        OptionT.liftF {
          for {
            result <- employeeService.delete(employeeId)
          } yield result.toString
        }

      case _ => OptionT.none
    }
}
