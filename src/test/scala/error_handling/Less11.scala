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

    "SecurityCodeTest" - {
      "valid security code: 4444" in {
        val initSecurityCode = "4444"
        val expected         = initSecurityCode.validNec
        val actual = for {
          secCodeRef <- validatePaymentCardSecurityCode(initSecurityCode)
          secCodeVal  = secCodeRef.value
        } yield secCodeVal
        assert(actual == expected)
      }
      "invalid security code: 111a" in {
        val initSecurityCode = "111a"
        val expected         = SecurityCodeIsInvalid.invalidNec
        val actual           = validatePaymentCardSecurityCode(initSecurityCode)
        assert(actual == expected)
      }
      "invalid security code: 11111" in {
        val initSecurityCode = "11111"
        val expected         = SecurityCodeIsInvalid.invalidNec
        val actual           = validatePaymentCardSecurityCode(initSecurityCode)
        assert(actual == expected)
      }
    }

    "ExpirationDateTest" - {
      "valid payment card expiration date: 2022-01-01" in {
        val initDate = "2022-01-01"
        val expected = Instant.parse(initDate.concat("T00:00:00Z")).validNec
        val actual   = validatePaymentCardExpirationDate(initDate)
        assert(actual == expected)
      }
      "invalid payment card expiration date: 2020-01-01" in {
        val initDate = "2020-01-01"
        val expected = ExpirationDateIsOutOfBounds.invalidNec
        val actual   = validatePaymentCardExpirationDate(initDate)
        assert(actual == expected)
      }
      "invalid payment card expiration date type: 2022.01.01" in {
        val initDate = "2022.01.01"
        val expected = ExpirationDateIsNotDate.invalidNec
        val actual   = validatePaymentCardExpirationDate(initDate)
        assert(actual == expected)
      }
    }

    "CardNumberTest" - {
      "valid payment card number: 1111222233334444" in {
        val initNumber = "1111222233334444"
        val expected   = initNumber.validNec
        val actual = for {
          cardNumRef <- validatePaymentCardNumber(initNumber)
          cardNumVal  = cardNumRef.value
        } yield cardNumVal
        assert(actual == expected)
      }
      "invalid payment card number: 11112222333344445" in {
        val initNumber = "11112222333344445"
        val expected   = CardNumberIsInvalid.invalidNec
        val actual     = validatePaymentCardNumber(initNumber)
        assert(actual == expected)
      }
      "invalid payment card number: aaaabbbbccccdddd" in {
        val initNumber = "aaaabbbbccccdddd"
        val expected   = CardNumberIsInvalid.invalidNec
        val actual     = validatePaymentCardNumber(initNumber)
        assert(actual == expected)
      }
    }

    "PaymentCardTest" - {
      "valid payment card: true" in {
        val initNumber       = "1111222233334444"
        val initDate         = "2022-01-01"
        val initSecurityCode = "4444"
        val initCard         = PaymentCardDTO(initNumber, initDate, initSecurityCode)
        val expected         = initCard.validNec
        val actual = for {
          cardRef <- validatePaymentCard(initCard)
          number   = cardRef.cardNumber.value
          date     = cardRef.expirationDate.toString.take(10)
          code     = cardRef.securityCode.value
        } yield PaymentCardDTO(number, date, code)
        assert(expected == actual)
      }
      "invalid payment card: all + date type" in {
        val initNumber       = "11112222333344445"
        val initDate         = "2022.01.01"
        val initSecurityCode = "aaaa"
        val initCard         = PaymentCardDTO(initNumber, initDate, initSecurityCode)
        val expected         = Invalid(Chain(CardNumberIsInvalid, ExpirationDateIsNotDate, SecurityCodeIsInvalid))
        val actual           = validatePaymentCard(initCard)
        assert(expected == actual)
      }
      "invalid payment card: all + date bound" in {
        val initNumber       = "11112222333344445"
        val initDate         = "2020-01-01"
        val initSecurityCode = "aaaa"
        val initCard         = PaymentCardDTO(initNumber, initDate, initSecurityCode)
        val expected         = Invalid(Chain(CardNumberIsInvalid, ExpirationDateIsOutOfBounds, SecurityCodeIsInvalid))
        val actual           = validatePaymentCard(initCard)
        assert(expected == actual)
      }
    }

  }

  "PersonValidator" - {

    "PassportNumberTest" - {
      "valid person passport number: 0123456789AA22" in {
        val initNumber = "0123456789AA22"
        val expected   = initNumber.validNec
        val actual = for {
          passNumRef <- validatePersonPassportNumber(initNumber)
          passNumStr  = passNumRef.value
        } yield passNumStr
        assert(actual == expected)
      }
      "invalid person passport number: 0123456789AAA1" in {
        val initNumber = "0123456789AAA1"
        val expected   = PassportNumberIsInvalid.invalidNec
        val actual     = validatePersonPassportNumber(initNumber)
        assert(actual == expected)
      }
    }

    "BirthDayTest" - {
      "valid person birthday: 2000-01-01" in {
        val initDate = "2000-01-01"
        val actual   = validatePersonBirthDay(initDate)
        val expected = Instant.parse(initDate.concat("T00:00:00Z")).validNec
        assert(actual == expected)
      }
      "invalid person birthday too young: 2010-01-01" in {
        val initDate = "2010-01-01"
        val actual   = validatePersonBirthDay(initDate)
        val expected = BirthDayIsOutOfBounds.invalidNec
        assert(actual == expected)
      }
      "invalid person birthday too old: 1940-01-01" in {
        val initDate = "1940-01-01"
        val actual   = validatePersonBirthDay(initDate)
        val expected = BirthDayIsOutOfBounds.invalidNec
        assert(actual == expected)
      }
      "invalid person birthday type: 2000.01.01" in {
        val initDate = "2000.01.01"
        val actual   = validatePersonBirthDay(initDate)
        val expected = BirthDayIsNotDate.invalidNec
        assert(actual == expected)
      }
    }

    "NameTest" - {
      "valid person name: Arty" in {
        val initName = "Arty"
        val expected = initName.validNec
        val actual = for {
          nameRef <- validatePersonName(initName)
          nameVal  = nameRef.value
        } yield nameVal
        assert(expected == actual)
      }
      "invalid person name first letter: arty" in {
        val initName = "arty"
        val expected = UsernameIsInvalid.invalidNec
        val actual   = validatePersonName(initName)
        assert(expected == actual)
      }
      "invalid person name length: Ar" in {
        val initName = "Ar"
        val expected = UsernameIsInvalid.invalidNec
        val actual   = validatePersonName(initName)
        assert(expected == actual)
      }
    }

    "PersonTest" - {
      "valid person: true" in {
        val initName    = "Arty"
        val initBirth   = "2000-01-01"
        val initPassNum = "0123456789AA22"
        val initPerson  = PersonDTO(initName, initBirth, initPassNum)
        val expected    = initPerson.valid
        val actual = for {
          personRef <- validatePerson(initPerson)
          name       = personRef.name.value
          birth      = personRef.birthDay.toString.take(10)
          passN      = personRef.passportNumber.value
        } yield PersonDTO(name, birth, passN)
        assert(expected == actual)
      }
      "invalid person: all + name letters number + birth type" in {
        val initName    = "Ar"
        val initBirth   = "2000.01.01"
        val initPassNum = "0123456789AAA1"
        val initPerson  = PersonDTO(initName, initBirth, initPassNum)
        val expected    = Invalid(Chain(UsernameIsInvalid, BirthDayIsNotDate, PassportNumberIsInvalid))
        val actual      = validatePerson(initPerson)
        assert(expected == actual)
      }
      "invalid person: all + name first letter + birth too young" in {
        val initName    = "arty"
        val initBirth   = "2010-01-01"
        val initPassNum = "0123456789AAA1"
        val initPerson  = PersonDTO(initName, initBirth, initPassNum)
        val expected    = Invalid(Chain(UsernameIsInvalid, BirthDayIsOutOfBounds, PassportNumberIsInvalid))
        val actual      = validatePerson(initPerson)
        assert(expected == actual)
      }
    }

  }

  "AccountValidator" - {
    "valid account: true" in {
      val initPerson  = PersonDTO("Arty", "2000-01-01", "0123456789AA22")
      val initCard    = PaymentCardDTO("1111222233334444", "2022-01-01", "4444")
      val initAccount = AccountDTO(initPerson, initCard)
      val expected    = initAccount.validNec
      val actual = for {
        accRef        <- validateAccount(initAccount)
        personNameVal  = accRef.person.name.value
        personBirthVal = accRef.person.birthDay.toString.take(10)
        personPassNVal = accRef.person.passportNumber.value
        cardNumVal     = accRef.card.cardNumber.value
        cardExpVal     = accRef.card.expirationDate.toString.take(10)
        cardSecCVal    = accRef.card.securityCode.value
      } yield AccountDTO(
        PersonDTO(personNameVal, personBirthVal, personPassNVal),
        PaymentCardDTO(cardNumVal, cardExpVal, cardSecCVal)
      )
      assert(actual == expected)
    }
    "invalid account: name length + date type" in {
      val initPerson  = PersonDTO("Ar", "2000.01.01", "0123456789AAA2")
      val initCard    = PaymentCardDTO("11112222333344445", "2022.01.01", "44445")
      val initAccount = AccountDTO(initPerson, initCard)
      val expected = Invalid(
        Chain(
          UsernameIsInvalid,
          BirthDayIsNotDate,
          PassportNumberIsInvalid,
          CardNumberIsInvalid,
          ExpirationDateIsNotDate,
          SecurityCodeIsInvalid
        )
      )
      val actual = validateAccount(initAccount)
      assert(actual == expected)
    }
    "invalid account: name first letter + date bound" in {
      val initPerson  = PersonDTO("arty", "2010-01-01", "0123456789AAA2")
      val initCard    = PaymentCardDTO("11112222333344445", "2020-01-01", "44445")
      val initAccount = AccountDTO(initPerson, initCard)
      val expected = Invalid(
        Chain(
          UsernameIsInvalid,
          BirthDayIsOutOfBounds,
          PassportNumberIsInvalid,
          CardNumberIsInvalid,
          ExpirationDateIsOutOfBounds,
          SecurityCodeIsInvalid
        )
      )
      val actual = validateAccount(initAccount)
      assert(actual == expected)
    }
  }

}
