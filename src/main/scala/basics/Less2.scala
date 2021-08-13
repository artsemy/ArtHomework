package basics

import scala.io.Source
import scala.io.StdIn

object Less2 extends App {

  sealed trait CellContainer

  object CellContainer {

    final case class NonNegativeInteger(i: Int) extends CellContainer
    final case class TextLine(textLine: String) extends CellContainer
    final case class EmptyCell(emptyLine: String) extends CellContainer
    final case class CellLink(link: String) extends CellContainer
    final case class WrongFormat(line: String) extends CellContainer
    final case class Expression(expression: String) extends CellContainer

    def apply(input: String): CellContainer = {
      input match {
        case x if x.toIntOption.isDefined && x.toInt > -1 => NonNegativeInteger(x.toInt)
        case x if x.charAt(0) == '\'' => TextLine(x)
        case x if x.charAt(0) == '=' => Expression(x)
        case x if x == " " => EmptyCell(x)
        case x if x.charAt(0) > 64 && x.charAt(0) < 91 => CellLink(x)
        case _ => WrongFormat("#format")
      }
    }
  }

  import CellContainer._

  final case class Counter(matrix: Map[String, CellContainer]) {
    val signDivider = "[-+/*]"
    val noSignDivider = "[^+/*-]+"

    def count(): Map[String, CellContainer] = {
      matrix.map(x => (x._1, countCell(x._2)))
    }

    def countCell(cellContainer: CellContainer): CellContainer = {
      cellContainer match {
        case TextLine(textLine) => CellContainer(textLine.tail)
        case Expression(expression) => countExpression(expression.tail)
        case x => x
      }
    }

    def countExpression(str: String): CellContainer = {
      val units = str.split(signDivider).map(x => CellContainer(x))
      val signs = str.split(noSignDivider).iterator
      if (units.length == 0) WrongFormat("#expression")
      else if (units.length == 1) units(0) match {
        case NonNegativeInteger(i) => NonNegativeInteger(i) //fix
        case CellLink(link) => countCell(matrix.getOrElse(link, WrongFormat("#link")))
        case _ => WrongFormat("#expression")
      }
      else units.reduce((x, y) => countPair(x, y, signs.next())) //works???
    }

    def countPair(s1: CellContainer, s2: CellContainer, mathOperation: String): CellContainer = {
      val p1 = s1 match {
        case NonNegativeInteger(i) => NonNegativeInteger(i)
        case CellLink(link) => countCell(matrix.getOrElse(link, WrongFormat("#link")))
        case _ => WrongFormat("#expression")
      }
      val p2 = s2 match {
        case NonNegativeInteger(i) => NonNegativeInteger(i)
        case CellLink(link) => countExpression(link)
        case _ => WrongFormat("#expression")
      }
      (p1, p2) match {
        case (NonNegativeInteger(i1), NonNegativeInteger(i2)) => mathOperation match {
          case "+" => NonNegativeInteger(i1 + i2)
          case "-" => NonNegativeInteger(i1 - i2)
          case "/" if i2 != 0 => NonNegativeInteger(i1 / i2)
          case "*" => NonNegativeInteger(i1 * i2)
          case _ => WrongFormat("#operation")
        }
        case _ => WrongFormat("expression")
      }
    }
  }

  final case class MyReader() {
    def readMatrix(): Map[String, CellContainer] = {
      val inputFilePath = readFilePathFromConsole()
      val source = Source.fromFile(inputFilePath)
      val iterator = source.getLines()

      val size = iterator.next().split('\t') //not used
      generateMatrix(iterator)
    }

    def readFilePathFromConsole(): String = {
      print("input file path: ")
      StdIn.readLine()
    }

    def generateMatrix(iterator: Iterator[String]): Map[String, CellContainer] = {
      val columns = (65 to 90).toList.map(x => x.toChar)
      if (iterator.hasNext) {

      }
    }
  }

}
