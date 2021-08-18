package basics

import scala.io.{BufferedSource, Source}
import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

object Less2 {

  sealed trait CellContainer {
    def value: String
  }

  object CellContainer {

    final case class NonNegativeIntegerCell(value: String) extends CellContainer
    final case class TextLineCell(value: String) extends CellContainer
    final case class EmptyCell(value: String) extends CellContainer
    final case class LinkCell(value: String) extends CellContainer
    final case class WrongFormatCell(value: String) extends CellContainer
    final case class ExpressionCell(value: String) extends CellContainer

    def apply(input: String): CellContainer = {
      input match {
        case x if x.toIntOption.isDefined && x.toInt > -1 => NonNegativeIntegerCell(x)
        case x if x.charAt(0) == '\'' => TextLineCell(x)
        case x if x.charAt(0) == '=' => ExpressionCell(x)
        case x if x == " " => EmptyCell(x)
        case x if x.charAt(0) > 64 && x.charAt(0) < 91 => LinkCell(x)
        case _ => WrongFormatCell("#format")
      }
    }
  }

  import CellContainer._

  type ErrorMessage = String
  final case class RawSpreadsheet(matrix: List[List[String]])
  final case class ProcessedSpreadsheet(matrix: List[List[CellContainer]])

  def validateReadPath(readPath: String): Either[ErrorMessage, BufferedSource] = {
    Try(Source.fromFile(readPath)) match {
      case Failure(exception) => Left(exception.getMessage)
      case Success(bufferedSource) => Right(bufferedSource)
    }
  }

  val PathNotExistsMessage = "path not exists"

  def validateWritePath(writePath: String): Either[ErrorMessage, String] = {
    val fileNameStartIndex = writePath.lastIndexOf("/") + 1
    val pathWithoutFileName = writePath.substring(0, fileNameStartIndex)

    if (Files.exists(Paths.get(pathWithoutFileName))) Right(writePath)
    else Left(PathNotExistsMessage)
  }

  trait SpreadsheetParser {
    def parse(): Either[ErrorMessage, RawSpreadsheet]
  }

  class LocalSpreadsheetParser(bufferedSource: BufferedSource) extends SpreadsheetParser {

    val WrongSizeFormatMessage = "wrong size format"
    val WrongElementNumberMessage = "wrong element number"
    val EmptyInputMessage = "empty input"

    override def parse(): Either[ErrorMessage, RawSpreadsheet] = {
      val lines = readLines()
      val initRows = lines.map(x => x.split("\t").toList)
      val validatedMatrix = validateInputRows(initRows)

      validatedMatrix match {
        case Left(value) => Left(value)
        case Right(matrix) => Right(RawSpreadsheet(matrix))
      }
    }

    def readLines(): List[String] = {
      val lines = bufferedSource.getLines().toList
      bufferedSource.close()
      lines
    }

    def validateInputRows(initRows: List[List[String]]): Either[ErrorMessage, List[List[String]]] = {
      initRows match {
        case sizeLine :: matrix => validateSizeAndMatrix(sizeLine, matrix)
        case _ => Left(EmptyInputMessage)
      }
    }

    def validateSizeAndMatrix(size: List[String],
                              matrix: List[List[String]]): Either[ErrorMessage, List[List[String]]] = {
      for {
        validatedSize <- validateSize(size)
        validatedMatrix <- validateMatrix(matrix, validatedSize)
      } yield validatedMatrix
    }

    def validateSize(sizeRow: List[String]): Either[ErrorMessage, (Int, Int)] = {
      sizeRow match {
        case firstElem :: lastElem :: Nil => validateTypeInt(firstElem, lastElem)
        case _ => Left(WrongSizeFormatMessage)
      }
    }

    def validateTypeInt(s1: String, s2: String): Either[ErrorMessage, (Int, Int)] = {
      (s1.toIntOption, s2.toIntOption) match {
        case (Some(x1), Some(x2)) => Right((x1, x2))
        case _ => Left(WrongSizeFormatMessage)
      }
    }

    def validateMatrix(list: List[List[String]], size: (Int, Int)): Either[ErrorMessage, List[List[String]]] = {
      val (rowNumber, columnNumber) = size

      if (list.length == rowNumber) {
        val rowSizes = list.map(x => x.length)
        if (rowSizes.forall(_ == columnNumber)) Right(list)
        else Left(WrongElementNumberMessage)
      } else Left(WrongElementNumberMessage)
    }
  }

  trait SpreadsheetProcessor {
    def process(spreadsheet: RawSpreadsheet): ProcessedSpreadsheet
  }

  class SimpleSpreadsheetProcessor extends SpreadsheetProcessor {
    val SignDivider = "[-+/*]"
    val NoSignDivider = "[^+/*-]+"
    val WrongFormatMessage = "#wrongFormat"
    val IntValueOfA: Int = 'A'.toInt
    val IndexCountDifference = 1

    override def process(spreadsheet: RawSpreadsheet): ProcessedSpreadsheet = {
      val startMatrix = spreadsheet.matrix.map(x => x.map(y => CellContainer(y)))
      val processedMatrix = processMatrix(startMatrix)

      ProcessedSpreadsheet(processedMatrix)
    }

    def processMatrix(list: List[List[CellContainer]]): List[List[CellContainer]] = {
      list.map(x => x.map(y => processElement(y, list)))
    }

    def processElement(cell: CellContainer, list: List[List[CellContainer]]): CellContainer = {
      cell match {
        case TextLineCell(value) => TextLineCell(value.tail)
        case ExpressionCell(value) => processExpressionCell(value.tail, list)
        case LinkCell(value) => processLinkCell(value, list)
        case x => x
      }
    }

    def processExpressionCell(expression: String, list: List[List[CellContainer]]): CellContainer = {
      val units = expression.split(SignDivider).map(x => CellContainer(x)).toList
      val signsIterator = expression.split(NoSignDivider).iterator

      units match {
        case Nil => WrongFormatCell(WrongFormatMessage)
        case oneElem :: Nil => processLinkCell(expression, list)
        case severalElements => signsIterator.next()
          units.reduceLeft((x, y) => processPairCell(x, y, signsIterator.next(), list)) //useful iterator
      }
    }

    def processLinkCell(str: String, list: List[List[CellContainer]]): CellContainer = {
      val indexColumn = str.head.toInt - IntValueOfA //try catch
      val indexRow = str.tail.toInt - IndexCountDifference //try catch
      val rowList = list(indexRow)
      val elem = rowList(indexColumn)

      processElement(elem, list)
    }

    def processPairCell(right: CellContainer, left: CellContainer, mathOperation: String,
                        list: List[List[CellContainer]]): CellContainer = {
      val operand1 = processElement(right, list)
      val operand2 = processElement(left, list)

      (operand1, operand2) match {
        case (NonNegativeIntegerCell(i1), NonNegativeIntegerCell(i2)) => countMathExpression(i1, i2, mathOperation)
        case _ => WrongFormatCell(WrongFormatMessage)
      }
    }

    def countMathExpression(left: String, right: String, mathOperation: String): CellContainer = {
      mathOperation match {
        case "+" => NonNegativeIntegerCell((left.toInt + right.toInt).toString)
        case "-" => NonNegativeIntegerCell((left.toInt - right.toInt).toString)
        case "/" if right.toInt != 0 => NonNegativeIntegerCell((left.toInt / right.toInt).toString)
        case "*" => NonNegativeIntegerCell((left.toInt * right.toInt).toString)
        case _ => WrongFormatCell(WrongFormatMessage)
      }
    }
  }

  trait SpreadsheetWriter {
    def write(writePath: String, processedSpreadsheet: ProcessedSpreadsheet): Either[ErrorMessage, ProcessedSpreadsheet]
  }

  class LocalSpreadsheetWriter() extends SpreadsheetWriter {
    override def write(writePath: String,
                       processedSpreadsheet: ProcessedSpreadsheet): Either[ErrorMessage, ProcessedSpreadsheet] = {

      val matrixToWrite = processedSpreadsheet.matrix.map(x => x.map(y => y.value))
      val fileWriter = new FileWriter(new File(writePath))
      val resultText = buildText(matrixToWrite)

      fileWriter.write(resultText)
      fileWriter.close()
      Right(processedSpreadsheet)
    }

    def buildText(matrix: List[List[String]]): String = {
      matrix
        .map(line => buildLine(line))
        .reduceLeft((x, y) => x + "\n" + y)
    }

    def buildLine(line: List[String]): String = {
      line.reduceLeft((x, y) => x + "\t" + y)
    }
  }

  def main(args: Array[String]): Unit = {
    val readPath = args(0)
    val writePath = args(1)

    val result = for {
      validReadPathSource <- validateReadPath(readPath)
      validWritePath <- validateWritePath(writePath)

      spreadsheetParser = new LocalSpreadsheetParser(validReadPathSource)
      parsedMatrix <- spreadsheetParser.parse()

      spreadsheetProcessor = new SimpleSpreadsheetProcessor
      processedSpreadsheet = spreadsheetProcessor.process(parsedMatrix)

      spreadsheetWriter = new LocalSpreadsheetWriter
      _ <- spreadsheetWriter.write(validWritePath, processedSpreadsheet)
    } yield ()

    result match {
      case Left(error) => println(error)
      case Right(_) =>
    }
  }

}
