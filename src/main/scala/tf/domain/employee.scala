package tf.domain

import money.Money
import workingPosition.WorkingPosition

import java.time.Instant
import java.util.UUID

object employee {

  final case class EmployeeId(value: UUID)

  final case class Employee(
    employeeId: EmployeeId,
    birthday:   Instant,
    firstName:  String,
    lastName:   String,
    salary:     Money,
    position:   WorkingPosition
  )

}
