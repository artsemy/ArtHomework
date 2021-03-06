package tf.domain

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import io.circe.generic.JsonCodec
import money.Money
import workingPosition.WorkingPosition
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.refined

import java.time.Instant
import java.util.UUID

object employee {

  final case class EmployeeId(value: UUID)

  final case class Employee(
    employeeId: EmployeeId,
    birthday:   Instant,
    firstName:  FirstName,
    lastName:   LastName,
    salary:     Money,
    position:   WorkingPosition
  )

  @JsonCodec
  final case class EmployeeDTO(
    employeeId: String,
    birthday:   String,
    firstName:  String,
    lastName:   String,
    salary:     String,
    position:   String
  )

  @JsonCodec
  final case class CreateEmployeeDTO(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  )

  type FirstName = String Refined MatchesRegex[W.`"""[A-Z][a-z]{2,28}"""`.T]
  type LastName  = String Refined MatchesRegex[W.`"""[A-Z][a-z]{2,29}"""`.T]

}
