package tf.validation

import eu.timepit.refined.refineV
import tf.domain.employee.*
import tf.domain.money.Money
import tf.domain.workingPosition.WorkingPosition
import tf.domain.workingPosition.WorkingPosition.{Junior, Middle, Senior}
import tf.validation.EmployeeValidator.EmployeeValidationError._

import java.time.Instant
import java.util.{Currency, UUID}

object EmployeeValidator {

  trait EmployeeValidationError extends Throwable

  object EmployeeValidationError {
    final case object EmployeeBirthdayFormat extends EmployeeValidationError
    final case object EmployeeFirstNameFormat extends EmployeeValidationError
    final case object EmployeeLastNameFormat extends EmployeeValidationError
    final case object SalaryAmountFormat extends EmployeeValidationError
    final case object SalaryCurrencyFormat extends EmployeeValidationError
    final case object EmployeePositionFormat extends EmployeeValidationError
    final case object EmployeeIdFormat extends EmployeeValidationError
  }

  def validate(employeeDTO: EmployeeDTO): Either[EmployeeValidationError, Employee] = for {
    id <- validateId(employeeDTO.employeeId)
    bd <- validateBirthDay(employeeDTO.birthday)
    fn <- validateFirstName(employeeDTO.firstName)
    ln <- validateLastName(employeeDTO.lastName)
    sl <- validateMoney(employeeDTO.salary)
    ps <- validatePosition(employeeDTO.position)
  } yield Employee(id, bd, fn, ln, sl, ps)

  def validateId(id: String): Either[EmployeeValidationError, EmployeeId] = {
    Either.cond(UUID.fromString(id).isInstanceOf[UUID], EmployeeId(UUID.fromString(id)), EmployeeIdFormat)
  }

  def validateBirthDay(birthday: String): Either[EmployeeValidationError, Instant] = {
    Either.cond(
      Instant.parse(birthday).isInstanceOf[Instant],
      Instant.parse(birthday),
      EmployeeBirthdayFormat
    )
  }

  def validateFirstName(firstName: String): Either[EmployeeValidationError, FirstName] = {
    val res: Either[String, FirstName] = refineV(firstName)
    res.left.map(_ => EmployeeFirstNameFormat)
  }

  def validateLastName(lastName: String): Either[EmployeeValidationError, LastName] = {
    val res: Either[String, FirstName] = refineV(lastName)
    res.left.map(_ => EmployeeLastNameFormat)
  }

  def validateMoney(money: String): Either[EmployeeValidationError, Money] = {
    val currencyS = money.replaceAll("[0-9]+.[0-9]{0,2}", "")
    val amountS   = money.replace(currencyS, "")
    for {
      amount <- Either.cond(
        BigDecimal(amountS).isInstanceOf[BigDecimal],
        BigDecimal(amountS),
        SalaryAmountFormat
      )
      currency <- Either.cond(
        Currency.getInstance(currencyS).isInstanceOf[Currency],
        Currency.getInstance(currencyS),
        SalaryCurrencyFormat
      )
    } yield Money(amount, currency)
  }

  def validatePosition(position: String): Either[EmployeeValidationError, WorkingPosition] = {
    position match {
      case JUNIOR => Right(Junior)
      case MIDDLE => Right(Middle)
      case SENIOR => Right(Senior)
      case _      => Left(EmployeePositionFormat)
    }
  }

  final private val JUNIOR = "junior"
  final private val MIDDLE = "middle"
  final private val SENIOR = "senior"

}
