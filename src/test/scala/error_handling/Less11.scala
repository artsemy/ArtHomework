package error_handling

import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import error_handling.Less9.AccountValidationError._
import error_handling.Less9.PaymentCardValidator._
import error_handling.Less9._
import eu.timepit.refined.api.RefType
import eu.timepit.refined.refineV
import org.scalatest.freespec.AnyFreeSpec
import java.time.Instant

import scala.annotation.nowarn

@nowarn
class Less11 extends AnyFreeSpec {

  "PaymentCardValidator" - {
    "valid security code: 4444" in {
      val validSecurityCode = "4444"
      val act               = validatePaymentCardSecurityCode(validSecurityCode)
      val exp               = validSecurityCode.validNec
      assert(act == exp)
    }
    "invalid security code: aaaa" in {
      val invalidSecurityCode = "aaaa"
      val act                 = validatePaymentCardSecurityCode(invalidSecurityCode)
      val exp                 = Invalid(Chain(SecurityCodeIsInvalid))
      assert(act == exp)
    }
    "invalid security code: 11111" in {
      val invalidSecurityCode = "11111"
      val act                 = validatePaymentCardSecurityCode(invalidSecurityCode)
      val exp                 = Invalid(Chain(SecurityCodeIsInvalid))
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
  }

}
