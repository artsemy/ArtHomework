package db

import java.util.UUID

object DbCommon {

  val employeeRandId1: UUID = UUID.randomUUID()
  val employeeRandId2: UUID = UUID.randomUUID()
  val employeeRandId3: UUID = UUID.randomUUID()

  val positionRandId1: UUID = UUID.randomUUID()
  val positionRandId2: UUID = UUID.randomUUID()
  val positionRandId3: UUID = UUID.randomUUID()

  val createTableEmployeeSql: String =
    """CREATE TABLE employees(
      |  id UUID PRIMARY KEY,
      |  birthday DATE NOT NULL,
      |  firstName VARCHAR(100) NOT NULL,
      |  lastName VARCHAR(100) NOT NULL,
      |  salary VARCHAR(100) NOT NULL,
      |  positionId UUID NOT NULL,
      |  isArchived BOOLEAN
      |  FOREIGN KEY (positionId) REFERENCES positions(id));""".stripMargin

  val createTableWorkPositionsSql: String =
    """CREATE TABLE positions
      |  id UUID PRIMARY KEY,
      |  position VARCHAR(100) NOT NULL);""".stripMargin

  val fullFillTablesSql: String =
    s"""
       |INSERT INTO positions (id, position) VALUES
       |  ('$positionRandId1', 'junior'),
       |  ('$positionRandId2', 'middle'),
       |  ('$positionRandId3', 'senior');
       |
       |INSERT INTO employees (id, birthday, firstName, lastName, salary, positionId, isArchived)
       |  ('$employeeRandId1', '2010-10-10', 'Arty', 'Arty', '100.00USD', '$positionRandId1', 'false'),
       |  ('$employeeRandId2', '2010-12-12', 'Barty', 'Barty', '500.00USD', '$positionRandId2', 'false'),
       |  ('$employeeRandId3', '2010-05-05', 'Carty', 'Carty', '1000.00USD', '$positionRandId3', 'false');
       |""".stripMargin

}
