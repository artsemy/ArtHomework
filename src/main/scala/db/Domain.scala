package db

import tf.domain.employee.Employee

final case class EmployeeDb(employee: Employee, isArchived: Boolean)
