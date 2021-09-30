package tf.validation

import tf.domain.employee.{Employee, EmployeeId}
import tf.domain.money.Money
import tf.domain.workingPosition.WorkingPosition
import tf.domain.workingPosition.WorkingPosition.{Junior, Middle, Senior}
import tf.validation.EmployeeValidator.EmployeePersonValidation._
import tf.validation.EmployeeValidator.EmployeeWorkValidation._

import java.time.Instant
import java.util.{Currency, UUID}

object EmployeeValidator {

  trait EmployeeValidationError extends Throwable

  trait EmployeePersonValidation extends EmployeeValidationError

  object EmployeePersonValidation {
    final case object EmployeeBirthdayFormat extends EmployeePersonValidation
    final case object EmployeeFirstNameFormat extends EmployeePersonValidation
    final case object EmployeeLastNameFormat extends EmployeePersonValidation
  }

  trait EmployeeWorkValidation extends EmployeeValidationError

  object EmployeeWorkValidation {
    final case object SalaryAmountFormat extends EmployeeWorkValidation
    final case object SalaryCurrencyFormat extends EmployeeWorkValidation
    final case object EmployeePositionFormat extends EmployeeWorkValidation
  }

  def validate(
    birthday:  String,
    firstName: String,
    lastname:  String,
    salary:    String,
    position:  String
  ): Either[EmployeeValidationError, Employee] = for {
    bd <- Either.cond(Instant.parse(birthday).isInstanceOf[Instant], Instant.parse(birthday), EmployeeBirthdayFormat)
    fn <- Either.cond(firstName.matches("[A-Z][a-z]+"), firstName, EmployeeFirstNameFormat)
    ln <- Either.cond(lastname.matches("[A-Z][a-z]+"), lastname, EmployeeLastNameFormat)
    sl <- validateMoney(salary)
    ps <- validatePosition(position)
  } yield Employee(EmployeeId(UUID.randomUUID()), bd, fn, ln, sl, ps) //fix id

  def validateMoney(money: String): Either[EmployeeValidationError, Money] = {
    val currencyS = money.replaceAll("[0-9]+.[0-9]+", "")
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

  def validatePosition(position: String): Either[EmployeeWorkValidation, WorkingPosition] = {
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
