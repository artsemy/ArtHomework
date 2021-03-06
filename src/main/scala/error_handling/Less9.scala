package error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import eu.timepit.refined.{refineV, W}
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.string.MatchesRegex

import java.time.Instant
import scala.util.Try

object Less9 {

  final case class PersonDTO(
    name:           String,
    birthDay:       String,
    passportNumber: String
  )

  final case class PaymentCardDTO(
    cardNumber:     String,
    expirationDate: String,
    securityCode:   String
  )

  type Name           = String Refined MatchesRegex[W.`"""[A-Z][a-z]{2,29}"""`.T] //Arty
  type PassportNumber = String Refined MatchesRegex[W.`"""\\d{10}[A-Z]{2}\\d{2}"""`.T] //1234567890AA22

  type CardNumber   = String Refined MatchesRegex[W.`"""\\d{16}"""`.T]
  type SecurityCode = String Refined MatchesRegex[W.`"""\\d{4}"""`.T]

  final case class AccountDTO(personDTO: PersonDTO, cardDTO: PaymentCardDTO)
  final case class Account(person: Person, card: PaymentCard)

  final case class Person(
    name:           Name,
    birthDay:       Instant,
    passportNumber: PassportNumber
  )

  final case class PaymentCard(
    cardNumber:     CardNumber,
    expirationDate: Instant,
    securityCode:   SecurityCode
  )

  sealed trait AccountValidationError

  object AccountValidationError {

    final case object UsernameIsInvalid extends AccountValidationError {
      override def toString: String = "Username must be 3-30 length and cannot contain special characters"
    }

    final case object BirthDayIsNotDate extends AccountValidationError {
      override def toString: String = "Birthday must be date"
    }
    final case object BirthDayIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Birthday must be in (01.01.1940 to 01.01.2010)"
    }

    final case object PassportNumberIsInvalid extends AccountValidationError {
      override def toString: String = "Passport number must be /10 digits 2 chars 2 digits/"
    }

    final case object CardNumberIsInvalid extends AccountValidationError {
      override def toString: String = "Card number must be a number and contains 16 digits"
    }

    final case object ExpirationDateIsNotDate extends AccountValidationError {
      override def toString: String = "Expiration date must be date"
    }
    final case object ExpirationDateIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Expiration date must be in (now to now + 3 years)"
    }

    final case object SecurityCodeIsInvalid extends AccountValidationError {
      override def toString: String = "Security code must be 4 digits"
    }

  }

  object AccountValidator {

    import PaymentCardValidator.validatePaymentCard
    import PersonValidator.validatePerson
    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validateAccount(accountDTO: AccountDTO): AllErrorsOr[Account] = (
      validatePerson(accountDTO.personDTO),
      validatePaymentCard(accountDTO.cardDTO)
    ).mapN(Account)

    def simpleRefValidation[T, P](
      parameter: T,
      error:     AccountValidationError
    )(
      implicit v: Validate[T, P]
    ): AllErrorsOr[Refined[T, P]] = {
      refineV(parameter).left
        .map(_ => error)
        .toValidatedNec
    }
  }

  object PersonValidator {
    import AccountValidationError._
    import AccountValidator._

    def validatePerson(person: PersonDTO): AllErrorsOr[Person] = (
      validatePersonName(person.name),
      validatePersonBirthDay(person.birthDay),
      validatePersonPassportNumber(person.passportNumber)
    ).mapN(Person)

    def validatePersonName(name: String): AllErrorsOr[Name] =
      simpleRefValidation(name, UsernameIsInvalid)

    def validatePersonBirthDay(birthDay: String): AllErrorsOr[Instant] = {
      val year18 = 567648000000L
      val year75 = 2365200000000L

      (Try(Instant.parse(birthDay.concat("T00:00:00.00Z"))).toOption match {
        case None => Left(BirthDayIsNotDate)
        case Some(value) =>
          val bD = value.toEpochMilli
          val cD = Instant.now().toEpochMilli
          if (cD - bD > year18 && cD - bD < year75) Right(value)
          else Left(BirthDayIsOutOfBounds)
      }).toValidatedNec
    }

    def validatePersonPassportNumber(passportNumber: String): AllErrorsOr[PassportNumber] =
      simpleRefValidation(passportNumber, PassportNumberIsInvalid)

  }

  object PaymentCardValidator {
    import AccountValidationError._
    import AccountValidator._

    def validatePaymentCard(card: PaymentCardDTO): AllErrorsOr[PaymentCard] = (
      validatePaymentCardNumber(card.cardNumber),
      validatePaymentCardExpirationDate(card.expirationDate),
      validatePaymentCardSecurityCode(card.securityCode)
    ).mapN(PaymentCard)

    def validatePaymentCardNumber(number: String): AllErrorsOr[CardNumber] =
      simpleRefValidation(number, CardNumberIsInvalid)

    def validatePaymentCardExpirationDate(expirationDate: String): AllErrorsOr[Instant] = {
      val year3 = 94608000000L

      (Try(Instant.parse(expirationDate.concat("T00:00:00Z"))).toOption match {
        case None => Left(ExpirationDateIsNotDate)
        case Some(value) =>
          val eD = value.toEpochMilli
          val cD = Instant.now().toEpochMilli
          if (eD > cD && eD - cD < year3) Right(value)
          else Left(ExpirationDateIsOutOfBounds)
      }).toValidatedNec
    }

    def validatePaymentCardSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] =
      simpleRefValidation(securityCode, SecurityCodeIsInvalid)

  }

//  def main(args: Array[String]): Unit = {
//    import AccountValidator.validateAccount
//
//    val personDTOValid      = PersonDTO("Arty", "2001-01-01", "0123456789AA22")
//    val paymentCardDTOValid = PaymentCardDTO("4444444444444444", "2022-01-01", "1111")
//
//    val personDTOInvalid      = PersonDTO("ar", "2015-01-01", "qwerqwerqw2aaa")
//    val paymentCardDTOInvalid = PaymentCardDTO("44444444444444445", "2025-01-01", "11111")
//
//    println(validateAccount(personDTOValid, paymentCardDTOValid))
//    println(validateAccount(personDTOInvalid, paymentCardDTOInvalid))
//  }

}
