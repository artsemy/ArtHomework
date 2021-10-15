package db

import cats.effect.{ExitCode, IO, IOApp}
import db.DbCommon.{createTableEmployeeSql, createTableWorkPositionsSql, fullFillTablesSql}
import doobie.ConnectionIO
import doobie.util.fragment.Fragment
import doobie._
import doobie.implicits._
import doobie.implicits.javasql._ //for date i think
import doobie.implicits.javatime._ //for Instant meta
import doobie.h2.implicits._ //for UUID
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.{refineV, W}
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.auto._
import tf.domain.employee.{FirstName, LastName}
import tf.domain.money.Money
import tf.domain.workingPosition.WorkingPosition

import java.util.{Currency, UUID}

object Less18 extends IOApp {

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] =
    DbTransactor
      .make[IO]
      .use { xa =>
        for {
          _ <- setup().transact(xa)
          x <- all.transact(xa)
          _  = println(x) //printLine

        } yield ()
      }
      .as(ExitCode.Success)

//  implicit val uuidMeta:    Meta[UUID]    = Meta[String].timap(UUID.fromString)(_.toString) //why need this?
//  implicit val instantMeta: Meta[Instant] = Meta[Date].timap(_.toInstant)(x => x.) //implicits help with this
  implicit val moneyMeta: Meta[Money] = Meta[String].timap(convertStringToMoney)(convertMoneyToString)
  implicit val positionMeta: Meta[WorkingPosition] = {
    Meta[String].timap(convertStringToPosition)(convertPositionToString)
  }
  implicit val firstNameMeta: Meta[FirstName] =
    Meta[String].timap(convertParameter[String, MatchesRegex[W.`"""[A-Z][a-z]{2,28}"""`.T]](_, "Name"))(_.value)

  implicit val lastNameMeta: Meta[LastName] =
    Meta[String].timap(convertParameter[String, MatchesRegex[W.`"""[A-Z][a-z]{2,29}"""`.T]](_, "Name"))(_.value)

  private def convertParameter[T, P](
    parameter: T,
    default:   Refined[T, P]
  )(
    implicit v: Validate[T, P]
  ): Refined[T, P] = {
    refineV(parameter).getOrElse(default)
  }

  def convertStringToMoney(money: String): Money = {
    val currencyS = money.replaceAll("[0-9]+.[0-9]{0,2}", "")
    val amountS   = money.replace(currencyS, "")
    Money(BigDecimal(amountS), Currency.getInstance(currencyS))
  }

  def convertMoneyToString(money: Money): String = money.amount.toString() ++ money.currency.toString

  final private val JUNIOR = "junior"
  final private val MIDDLE = "middle"
  final private val SENIOR = "senior"

  def convertPositionToString(workingPosition: WorkingPosition): String = {
    workingPosition match {
      case WorkingPosition.Junior => JUNIOR
      case WorkingPosition.Middle => MIDDLE
      case WorkingPosition.Senior => SENIOR
    }
  }

  def convertStringToPosition(workingPosition: String): WorkingPosition = {
    workingPosition match {
      case JUNIOR => WorkingPosition.Junior
      case MIDDLE => WorkingPosition.Middle
      case SENIOR => WorkingPosition.Senior
    }
  }

  val ddl1: Fragment = Fragment.const(createTableWorkPositionsSql)
  val ddl2: Fragment = Fragment.const(createTableEmployeeSql)
  val dml:  Fragment = Fragment.const(fullFillTablesSql)

  def setup(): ConnectionIO[Unit] = for {
    _ <- ddl1.update.run
    _ <- ddl2.update.run
    _ <- dml.update.run
  } yield ()

  val selectAllFromEmployee =
    fr"""SELECT e.id, e.birthday, e.firstName, e.lastName, e.salary, p.position, e.isArchived FROM
        employees e INNER JOIN positions p ON e.positionId = p.id"""

  def all: doobie.ConnectionIO[List[EmployeeDb]] = {
    selectAllFromEmployee.query[EmployeeDb].to[List]
  }

  def create(employeeDb: EmployeeDb): ConnectionIO[Int] = {
    val emp = employeeDb.employee
    val fr =
      fr"""INSERT INTO employees (id, birthday, firstName, lastName, salary, positionId, isArchived)
        (${emp.employeeId.value}, ${emp.birthday}, ${emp.firstName}, ${emp.lastName}, ${emp.salary}, ${emp.position}, ${employeeDb.isArchived})
        """
    fr.update.run
  }

  def update(employeeDb: EmployeeDb): ConnectionIO[Int] = {
    val emp = employeeDb.employee
    val fr =
      fr"""UPDATE employees
        SET
        birthday = ${emp.birthday},
        firstName = ${emp.firstName},
        lastName = ${emp.lastName},
        salary = ${emp.salary},
        positionId = ${emp.position},
        isArchived = ${employeeDb.isArchived}
        WHERE id = ${emp.employeeId.value}"""
    fr.update.run
  }

  def find(idS: String): ConnectionIO[List[EmployeeDb]] = {
    val id = UUID.fromString(idS)
    val fr = selectAllFromEmployee ++ fr"""WHERE id = $id"""
    fr.query[EmployeeDb].to[List]
  }

  def delete(idS: String): ConnectionIO[Int] = {
    val id = UUID.fromString(idS)
    val fr = fr"""DELETE FROM employees WHERE id = $id"""
    fr.update.run
  }

}
