package error_handling

import cats.data.ValidatedNec
import cats.syntax.all.*
import java.util.Date

object Less9 {
  // think about adding Refined integration here to provide type level validation
  final case class Account(person: Person, card: PaymentCard)
  final case class Person(name: String, age: Int, birthDay: Date, passportNumber: String) // name, age, birthDay, passportNumber
  final case class PaymentCard(cardNumber: Long, expirationDate: Date, securityCode: Int) // card number, expirationDate, securityCode etc

  sealed trait AccountValidationError
  object AccountValidationError {
    final case object UsernameLengthIsInvalid extends AccountValidationError {
      override def toString: String = "Username must be between 3 and 30 characters"
    }
    final case object UsernameHasSpecialCharacters extends AccountValidationError {
      override def toString: String = "Username cannot contain special characters"
    }

    final case object AgeIsNotNumeric extends AccountValidationError {
      override def toString: String = "Age must be a number"
    }
    final case object AgeIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Student must be of age 18 to 75"
    }

    final case object BirthDayIsNotDate extends AccountValidationError {
      override def toString: String = "Birthday must be date"
    }
    final case object BirthDayIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Birthday must be in (01.01.1940 to 01.01.2010)"
    }

    final case object PassportNumberWrongFormat extends AccountValidationError {
      override def toString: String = "Passport number must be /10 digits 2 chars 2 digits/"
    }

    final case object CardNumberIsNotNumeric extends AccountValidationError {
      override def toString: String = "Card number must be a number"
    }
    final case object CardNumberLengthIsInvalid extends AccountValidationError {
      override def toString: String = "Card number must be 16 digits"
    }
    final case object CardNumberIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Card number must be from 0000,0000,0000,0000 to 9999,9999,9999,9999"
    }

    final case object ExpirationDateIsNotDate extends AccountValidationError {
      override def toString: String = "Expiration date must be date"
    }
    final case object ExpirationDateIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Expiration date must be in (now to now + 3 years)"
    }

    final case object SecurityCodeIsNotNumeric extends AccountValidationError {
      override def toString: String = "Security code must be a number"
    }
    final case object SecurityCodeLengthIsInvalid extends AccountValidationError {
      override def toString: String = "Security code must be 4 digits"
    }
    final case object SecurityCodeIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Security code must be from 0000 to 9999"
    }

  }

  object AccountValidator {

    import PersonValidator.validatePerson
    import PaymentCardValidator.validatePaymentCard
    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validate(person: Person, card: PaymentCard): AllErrorsOr[Account] = (
      validatePerson(person),
      validatePaymentCard(card)
    ).mapN(Account)

  }

  object PersonValidator {
    import AccountValidator.AllErrorsOr
    import AccountValidationError.*

    def validatePerson(person: Person): AllErrorsOr[Person] = (
      validatePersonName(person.name),
      validatePersonAge(person.age),
      validatePersonBirthDay(person.birthDay),
      validatePersonPassportNumber(person.passportNumber)
      ).mapN(Person)

    def validatePersonName(name: String): AllErrorsOr[String] = {
      def validatePersonNameLength: AllErrorsOr[String] = {
        if (name.length > 3 && name.length < 30) name.validNec
        else UsernameLengthIsInvalid.invalidNec
      }

      def  validatePersonNameContents: AllErrorsOr[String] = {
        if (name.matches("[a-zA-z]{3,30}")) name.validNec
        else UsernameHasSpecialCharacters.invalidNec
      }

      validatePersonNameLength *> validatePersonNameContents
    }

    def validatePersonAge(age: Int): AllErrorsOr[Int] = {
      def validatePersonAgeType: AllErrorsOr[Int] = age.validNec //fix everywhere!!!

      def validatePersonAgeBounds: AllErrorsOr[Int] = {
        if (age > 18 && age < 75) age.validNec
        else AgeIsOutOfBounds.invalidNec
      }

      validatePersonAgeType *> validatePersonAgeBounds
    }

    def validatePersonBirthDay(birthDay: Date): AllErrorsOr[Date] = {
      def validatePersonBirthDayType: AllErrorsOr[Date] = birthDay.validNec

      def validatePersonBirthDayBounds: AllErrorsOr[Date] = {
//        birthDay.after(new Date("1940-01-01")) && birthDay.before(new Date("2010-01-01"))
        if (birthDay.after(new Date(-946782000L)) && birthDay.before(new Date(1262296800L))) birthDay.validNec
        else BirthDayIsOutOfBounds.invalidNec
      }

      validatePersonBirthDayType *> validatePersonBirthDayBounds
    }

    def validatePersonPassportNumber(passportNumber: String): AllErrorsOr[String] = {
      def validatePersonPassportNumberContent: AllErrorsOr[String] = {
        if (passportNumber.matches("[0-9]{10}[A-Z]{2}[0-9]{2}")) passportNumber.validNec
        else PassportNumberWrongFormat.invalidNec
      }

      validatePersonPassportNumberContent
    }
  }

  object PaymentCardValidator {
    import AccountValidator.AllErrorsOr
    import AccountValidationError.*

    def validatePaymentCard(card: PaymentCard): AllErrorsOr[PaymentCard] = (
      validatePaymentCardNumber(card.cardNumber),
      validatePaymentCardExpirationDate(card.expirationDate),
      validatePaymentCardSecurityCode(card.securityCode)
    ).mapN(PaymentCard)

    def validatePaymentCardNumber(number : Long): AllErrorsOr[Long] = {
      def validatePaymentCardNumberType: AllErrorsOr[Long] = number.validNec

      def validatePaymentCardNumberLength: AllErrorsOr[Long] = {
        if (number > -1 && number < 10000000000000000L) number.validNec
        else CardNumberLengthIsInvalid.invalidNec
      }

      def validatePaymentCardNumberContent: AllErrorsOr[Long] = {
        if (number > -1 && number < 10000000000000000L) number.validNec
        else CardNumberIsOutOfBounds.invalidNec
      }

      validatePaymentCardNumberType *> validatePaymentCardNumberLength *> validatePaymentCardNumberContent
    }

    def validatePaymentCardExpirationDate(expirationDate : Date): AllErrorsOr[Date] = {
      def validatePaymentCardExpirationDateType: AllErrorsOr[Date] = expirationDate.validNec

      def validatePaymentCardExpirationDateBounds: AllErrorsOr[Date] = {
        if (expirationDate.after(new Date(new Date().getTime)) &&
          expirationDate.before(new Date(new Date().getTime + 31536000))) expirationDate.validNec
        else BirthDayIsOutOfBounds.invalidNec
      }

      validatePaymentCardExpirationDateType *> validatePaymentCardExpirationDateBounds
    }

    def validatePaymentCardSecurityCode(securityCode : Int): AllErrorsOr[Int] = {
      def validatePaymentCardSecurityCodeType: AllErrorsOr[Int] = securityCode.validNec

      def validatePaymentCardSecurityCodeLength: AllErrorsOr[Int] = {
        if (securityCode > -1 && securityCode < 100000) securityCode.validNec
        else SecurityCodeLengthIsInvalid.invalidNec
      }

      def validatePaymentCardSecurityCodeContent: AllErrorsOr[Int] = {
        if (securityCode > -1 && securityCode < 100000) securityCode.validNec
        else SecurityCodeIsOutOfBounds.invalidNec
      }

      validatePaymentCardSecurityCodeType *> validatePaymentCardSecurityCodeLength *>
        validatePaymentCardSecurityCodeContent
    }
  }

  def main(args: Array[String]): Unit = {
    import AccountValidator.validate
    val personValid = Person("arty", 20, new Date(978300000L), "qwerqwerqwer22aa") //"2001-01-01"
    val paymentCardValid = PaymentCard(4444444444444444L, new Date(1640984400L), 1111) //"2022-01-01"
    val accountValid = Account(personValid, paymentCardValid)

    val personInvalid = Person("ar", 17, new Date(1420059600L), "qwerqwerqw2222aa") //"2015-01-01"
    val paymentCardInvalid = PaymentCard(44444444444444445L, new Date(1735678800L), 1111) //"2025-01-01"
    val accountInvalid = Account(personInvalid, paymentCardInvalid)

    println(validate(personValid, paymentCardValid))
    println(validate(personInvalid, paymentCardInvalid))
  }

}
