package error_handling

import cats.data.Chain
import cats.data.Validated.Invalid
import cats.implicits._
import error_handling.Less9.AccountValidationError._
import error_handling.Less9.PaymentCardValidator._
import error_handling.Less9.AccountValidator._
import error_handling.Less9.PersonValidator._
import error_handling.Less9._
import org.scalatest.freespec.AnyFreeSpec

import java.time.Instant

class Less11 extends AnyFreeSpec {

  "PaymentCardValidator" - {
    "valid security code: 4444" in {
      val validSecurityCode = "4444"
      val exp               = validSecurityCode.validNec
      val act = for {
        secCodeRef <- validatePaymentCardSecurityCode(validSecurityCode)
        secCodeVal  = secCodeRef.value
      } yield secCodeVal
      assert(act == exp)
    }
    "invalid security code: aaaa" in {
      val invalidSecurityCode = "aaaa"
      val exp                 = SecurityCodeIsInvalid.invalidNec
      val act = for {
        secCodeRef <- validatePaymentCardSecurityCode(invalidSecurityCode)
        secCodeVal  = secCodeRef.value
      } yield secCodeVal
      assert(act == exp)
    }
    "invalid security code: 11111" in { //better than previous???
      val invalidSecurityCode = "11111"
      val act                 = validatePaymentCardSecurityCode(invalidSecurityCode)
      val exp                 = SecurityCodeIsInvalid.invalidNec
      assert(act == exp)
    }

    "valid payment card expiration date: 2022-01-01" in {
      val validDate = "2022-01-01"
      val act       = validatePaymentCardExpirationDate(validDate)
      val exp       = Instant.parse(validDate.concat("T00:00:00Z")).validNec
      assert(act == exp)
    }
    "invalid payment card expiration date: 2020-01-01" in {
      val invalidDate = "2020-01-01"
      val act         = validatePaymentCardExpirationDate(invalidDate)
      val exp         = ExpirationDateIsOutOfBounds.invalidNec
      assert(act == exp)
    }
    "invalid payment card expiration date type: 2022.01.01" in {
      val invalidDate = "2022.01.01"
      val act         = validatePaymentCardExpirationDate(invalidDate)
      val exp         = ExpirationDateIsNotDate.invalidNec
      assert(act == exp)
    }

    "valid payment card number: 1111222233334444" in {
      val validNumber = "1111222233334444"
      val exp         = validNumber.validNec
      val act = for {
        cardNumRef <- validatePaymentCardNumber(validNumber)
        cardNumVal  = cardNumRef.value
      } yield cardNumVal
      assert(act == exp)
    }
    "invalid payment card number: 11112222333344445" in {
      val invalidNumber = "11112222333344445"
      val exp           = CardNumberIsInvalid.invalidNec
      val act = for {
        cardNumRef <- validatePaymentCardNumber(invalidNumber)
        cardNumVal  = cardNumRef.value
      } yield cardNumVal
      assert(act == exp)
    }
    "invalid payment card number: aaaabbbbccccdddd" in {
      val validNumber = "aaaabbbbccccdddd"
      val exp         = CardNumberIsInvalid.invalidNec
      val act = for {
        cardNumRef <- validatePaymentCardNumber(validNumber)
        cardNumVal  = cardNumRef.value
      } yield cardNumVal
      assert(act == exp)
    }

    "valid payment card: true" in {
      val validNumber       = "1111222233334444"
      val validDate         = "2022-01-01"
      val validSecurityCode = "4444"
      val validCard         = PaymentCardDTO(validNumber, validDate, validSecurityCode)
      val exp               = validCard.validNec
      val act = for {
        cardRef <- validatePaymentCard(validCard)
        number   = cardRef.cardNumber.value
        date     = cardRef.expirationDate.toString.take(10)
        code     = cardRef.securityCode.value
      } yield PaymentCardDTO(number, date, code)
      assert(exp == act)
    }
    "invalid payment card: all + date type" in {
      val validNumber       = "11112222333344445"
      val validDate         = "2022.01.01"
      val validSecurityCode = "aaaa"
      val validCard         = PaymentCardDTO(validNumber, validDate, validSecurityCode)
      val exp               = Invalid(Chain(CardNumberIsInvalid, ExpirationDateIsNotDate, SecurityCodeIsInvalid))
      val act = for {
        cardRef <- validatePaymentCard(validCard)
        number   = cardRef.cardNumber.value
        date     = cardRef.expirationDate.toString.take(10)
        code     = cardRef.securityCode.value
      } yield PaymentCardDTO(number, date, code)
      assert(exp == act)
    }
    "invalid payment card: all + date bound" in {
      val validNumber       = "11112222333344445"
      val validDate         = "2020-01-01"
      val validSecurityCode = "aaaa"
      val validCard         = PaymentCardDTO(validNumber, validDate, validSecurityCode)
      val exp               = Invalid(Chain(CardNumberIsInvalid, ExpirationDateIsOutOfBounds, SecurityCodeIsInvalid))
      val act = for {
        cardRef <- validatePaymentCard(validCard)
        number   = cardRef.cardNumber.value
        date     = cardRef.expirationDate.toString.take(10)
        code     = cardRef.securityCode.value
      } yield PaymentCardDTO(number, date, code)
      assert(exp == act)
    }
  }

  "PersonValidator" - {
    "valid person passport number: 0123456789AA22" in {
      val validNumber = "0123456789AA22"
      val exp         = validNumber.validNec
      val act = for {
        passNumRef <- validatePersonPassportNumber(validNumber)
        passNumStr  = passNumRef.value
      } yield passNumStr
      assert(act == exp)
    }
    "invalid person passport number: 0123456789AAA1" in {
      val invalidNumber = "0123456789AAA1"
      val exp           = PassportNumberIsInvalid.invalidNec
      val act = for {
        passNumRef <- validatePersonPassportNumber(invalidNumber)
        passNumStr  = passNumRef.value
      } yield passNumStr
      assert(act == exp)
    }

    "valid person birthday: 2000-01-01" in {
      val validDate = "2000-01-01"
      val act       = validatePersonBirthDay(validDate)
      val exp       = Instant.parse(validDate.concat("T00:00:00Z")).validNec
      assert(act == exp)
    }
    "invalid person birthday too young: 2010-01-01" in {
      val validDate = "2010-01-01"
      val act       = validatePersonBirthDay(validDate)
      val exp       = BirthDayIsOutOfBounds.invalidNec
      assert(act == exp)
    }
    "invalid person birthday too old: 1940-01-01" in {
      val validDate = "1940-01-01"
      val act       = validatePersonBirthDay(validDate)
      val exp       = BirthDayIsOutOfBounds.invalidNec
      assert(act == exp)
    }
    "invalid person birthday type: 2000.01.01" in {
      val validDate = "2000.01.01"
      val act       = validatePersonBirthDay(validDate)
      val exp       = BirthDayIsNotDate.invalidNec
      assert(act == exp)
    }

    "valid person name: Arty" in {
      val validName = "Arty"
      val exp       = validName.validNec
      val act = for {
        nameRef <- validatePersonName(validName)
        nameVal  = nameRef.value
      } yield nameVal
      assert(exp == act)
    }
    "invalid person name first letter: arty" in {
      val invalidName = "arty"
      val exp         = UsernameIsInvalid.invalidNec
      val act = for {
        nameRef <- validatePersonName(invalidName)
        nameVal  = nameRef.value
      } yield nameVal
      assert(exp == act)
    }
    "invalid person name length: Ar" in {
      val invalidName = "Ar"
      val exp         = UsernameIsInvalid.invalidNec
      val act = for {
        nameRef <- validatePersonName(invalidName)
        nameVal  = nameRef.value
      } yield nameVal
      assert(exp == act)
    }

    "valid person: true" in {
      val validName    = "Arty"
      val validBirth   = "2000-01-01"
      val validPassNum = "0123456789AA22"
      val validPerson  = PersonDTO(validName, validBirth, validPassNum)
      val exp          = validPerson.valid
      val act = for {
        personRef <- validatePerson(validPerson)
        name       = personRef.name.value
        birth      = personRef.birthDay.toString.take(10)
        passN      = personRef.passportNumber.value
      } yield PersonDTO(name, birth, passN)
      assert(exp == act)
    }
    "invalid person: all + name letters number + birth type" in {
      val validName    = "Ar"
      val validBirth   = "2000.01.01"
      val validPassNum = "0123456789AAA1"
      val validPerson  = PersonDTO(validName, validBirth, validPassNum)
      val exp          = Invalid(Chain(UsernameIsInvalid, BirthDayIsNotDate, PassportNumberIsInvalid))
      val act = for {
        personRef <- validatePerson(validPerson)
        name       = personRef.name.value
        birth      = personRef.birthDay.toString.take(10)
        passN      = personRef.passportNumber.value
      } yield PersonDTO(name, birth, passN)
      assert(exp == act)
    }
    "invalid person: all + name first letter + birth too young" in {
      val validName    = "arty"
      val validBirth   = "2010-01-01"
      val validPassNum = "0123456789AAA1"
      val validPerson  = PersonDTO(validName, validBirth, validPassNum)
      val exp          = Invalid(Chain(UsernameIsInvalid, BirthDayIsOutOfBounds, PassportNumberIsInvalid))
      val act = for {
        personRef <- validatePerson(validPerson)
        name       = personRef.name.value
        birth      = personRef.birthDay.toString.take(10)
        passN      = personRef.passportNumber.value
      } yield PersonDTO(name, birth, passN)
      assert(exp == act)
    }
  }

}
