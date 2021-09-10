package error_handling

import cats.data.ValidatedNec
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.string.MatchesRegex

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Calendar

object Less9 {

  final case class PersonDTO(
    name:           String,
    age:            String,
    birthDay:       String,
    passportNumber: String
  )

  final case class PaymentCardDTO(
    cardNumber:     String,
    expirationDate: String,
    securityCode:   String
  )

  type Name           = String Refined MatchesRegex[W.`"""[A-Z][a-z]{2,29}"""`.T] //Arty
  type Age            = Int Refined Interval.Closed[W.`18`.T, W.`75`.T] //18-75
  type PassportNumber = String Refined MatchesRegex[W.`"""\\d{10}[A-Z]{2}\\d{2}"""`.T] //1234567890AA22

  type CardNumber   = String Refined MatchesRegex[W.`"""\\d{16}"""`.T]
  type SecurityCode = String Refined MatchesRegex[W.`"""\\d{4}"""`.T]

  final case class Account(person: Person, card: PaymentCard)

  final case class Person(
    name:           Name,
    age:            Age,
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

    final case object AgeIsNotNumeric extends AccountValidationError {
      override def toString: String = "Age must be a number"
    }
    final case object AgeIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Age must be from 18 to 75"
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

    import PersonValidator.validatePerson
    import PaymentCardValidator.validatePaymentCard
    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validate(person: PersonDTO, card: PaymentCardDTO): AllErrorsOr[Account] = (
      validatePerson(person),
      validatePaymentCard(card)
    ).mapN(Account)

  }

  object PersonValidator {
    import AccountValidator.AllErrorsOr
    import AccountValidationError._

    def validatePerson(person: PersonDTO): AllErrorsOr[Person] = (
      validatePersonName(person.name),
      validatePersonAge(person.age),
      validatePersonBirthDay(person.birthDay),
      validatePersonPassportNumber(person.passportNumber)
    ).mapN(Person)

    def validatePersonName(name: String): AllErrorsOr[Name] = {
      RefType
        .applyRef[Name](name)
        .left
        .map(_ => UsernameIsInvalid)
        .toValidatedNec
    }

    def validatePersonAge(age: String): AllErrorsOr[Age] = {
      (age.toIntOption match {
        case None => Left(AgeIsNotNumeric)
        case Some(value) =>
          RefType
            .applyRef[Age](value)
            .left
            .map(_ => AgeIsOutOfBounds)
      }).toValidatedNec
    }

    def validatePersonBirthDay(birthDay: String): AllErrorsOr[DateStr] = {
      val year18 = 567648000000L
      val year75 = 2365200000000L

      (RefType
        .applyRef[DateStr](birthDay) match {
        case Left(_) => Left(BirthDayIsNotDate)
        case Right(value) =>
          val formatter = new SimpleDateFormat("yyyy-MM-dd")
          val bD        = formatter.parse(value).getTime
          val cD        = Calendar.getInstance().getTime.getTime
          if (cD - bD > year18 && cD - bD < year75) Right(value)
          else Left(BirthDayIsOutOfBounds)
      }).toValidatedNec
    }

    def validatePersonPassportNumber(passportNumber: String): AllErrorsOr[PassportNumber] = {
      RefType
        .applyRef[PassportNumber](passportNumber)
        .left
        .map(_ => PassportNumberIsInvalid)
        .toValidatedNec
    }

  }

  object PaymentCardValidator {
    import AccountValidator.AllErrorsOr
    import AccountValidationError.*

    def validatePaymentCard(card: PaymentCardDTO): AllErrorsOr[PaymentCard] = (
      validatePaymentCardNumber(card.cardNumber),
      validatePaymentCardExpirationDate(card.expirationDate),
      validatePaymentCardSecurityCode(card.securityCode)
    ).mapN(PaymentCard)

    def validatePaymentCardNumber(number: String): AllErrorsOr[CardNumber] = {
      RefType
        .applyRef[CardNumber](number)
        .left
        .map(_ => CardNumberIsInvalid)
        .toValidatedNec
    }

    def validatePaymentCardExpirationDate(expirationDate: String): AllErrorsOr[DateStr] = {
      val year3 = 94608000000L

      (RefType
        .applyRef[DateStr](expirationDate) match {
        case Left(_) => Left(ExpirationDateIsNotDate)
        case Right(value) =>
          val formatter = new SimpleDateFormat("yyyy-MM-dd")
          val eD        = formatter.parse(value).getTime
          val cD        = Instant.now.toEpochMilli
          if (eD - cD < year3) Right(value)
          else Left(ExpirationDateIsOutOfBounds)
      }).toValidatedNec
    }

    def validatePaymentCardSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] = {
      RefType
        .applyRef[SecurityCode](securityCode)
        .left
        .map(_ => SecurityCodeIsInvalid)
        .toValidatedNec
    }
  }

  def main(args: Array[String]): Unit = {
    import AccountValidator.validate

    val personDTOValid      = PersonDTO("Arty", "20", "2001-01-01", "0123456789AA22")
    val paymentCardDTOValid = PaymentCardDTO("4444444444444444", "2022-01-01", "1111")

    val personDTOInvalid      = PersonDTO("ar", "17a", "2015-01-01", "qwerqwerqw2aaa")
    val paymentCardDTOInvalid = PaymentCardDTO("44444444444444445", "2025-01-01", "11111")

    println(validate(personDTOValid, paymentCardDTOValid))
    println(validate(personDTOInvalid, paymentCardDTOInvalid))
  }

}
