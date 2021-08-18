package basics

import scala.io.{BufferedSource, Source}
import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}
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

  def validateWritePath(writePath: String): Either[ErrorMessage, String] = {
    val fileNameStartIndex = writePath.lastIndexOf("/") + 1
    val pathWithoutFileName = writePath.substring(0, fileNameStartIndex)

    if (Files.exists(Paths.get(pathWithoutFileName))) Right(writePath)
    else Left("path not exists")
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
      val initMatrix = lines.map(x => x.split("\t").toList)
      val parsedMatrix = validateMatrixWithSize(initMatrix)

      parsedMatrix match {
        case Left(value) => Left(value)
        case Right(matrix) => Right(RawSpreadsheet(matrix))
      }
    }

    def readLines(): List[String] = {
      val lines = bufferedSource.getLines().toList
      bufferedSource.close()
      lines
    }

    def validateMatrixWithSize(matrixWithSize: List[List[String]]): Either[ErrorMessage, List[List[String]]] = {
      val iterator = matrixWithSize.iterator
      if (iterator.hasNext) {
        val size = validateInputSize(iterator.next())
        size match {
          case Right(value) => validateMatrix(iterator.toList, value)
          case Left(value) => Left(value)
        }
      } else Left(EmptyInputMessage)
    }

    def validateInputSize(strings: List[String]): Either[ErrorMessage, (Int, Int)] = {
      if (strings.length == 2) {
        Try(strings.map(x => x.toInt)) match {
          case Success(value) => Right((value.head, value.tail.head)) // 1st and 2nd
          case Failure(_) => Left(WrongSizeFormatMessage)
        }
      } else
        Left(WrongSizeFormatMessage)
    }

    def validateMatrix(array: List[List[String]], size: (Int, Int)): Either[ErrorMessage, List[List[String]]] = {
      val (lineNumber, columnNumber) = size
      if (array.length == lineNumber) {
        val lineSizes = array.map(x => x.length)
        if (lineSizes.forall(x => x == columnNumber)) Right(array)
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

    def processMatrix(array: List[List[CellContainer]]): List[List[CellContainer]] = {
      array.map(x => x.map(y => processElement(y, array)))
    }

    def processElement(cell: CellContainer, array: List[List[CellContainer]]): CellContainer = {
      cell match {
        case TextLineCell(value) => TextLineCell(value.tail)
        case ExpressionCell(value) => processExpressionCell(value.tail, array)
        case LinkCell(value) => processLinkCell(value, array)
        case x => x
      }
    }

    def processExpressionCell(str: String, array: List[List[CellContainer]]): CellContainer = {
      val units = str.split(SignDivider).map(x => CellContainer(x))
      val signsIterator = str.split(NoSignDivider).iterator
      if (units.isEmpty) WrongFormatCell(WrongFormatMessage)
      else if (units.tail.isEmpty) processLinkCell(str, array) //units.length == 1
      else {
        signsIterator.next()
        units.reduceLeft((x, y) => processPairCell(x, y, signsIterator.next(), array))
      }
    }

    def processLinkCell(str: String, array: List[List[CellContainer]]): CellContainer = {
      val indexColumn = str.head.toInt - IntValueOfA //try match
      val indexLine = str.tail.toInt - IndexCountDifference //try match
      val lineArr = array(indexLine)
      val elem = lineArr(indexColumn)
      processElement(elem, array)
    }

    def processPairCell(right: CellContainer, left: CellContainer, mathOperation: String, array: List[List[CellContainer]]): CellContainer = {
      val operand1 = processElement(right, array)
      val operand2 = processElement(left, array)
      (operand1, operand2) match {
        case (NonNegativeIntegerCell(i1), NonNegativeIntegerCell(i2)) =>
          mathOperation match {
            case "+" => NonNegativeIntegerCell((i1.toInt + i2.toInt).toString)
            case "-" => NonNegativeIntegerCell((i1.toInt - i2.toInt).toString)
            case "/" if i2.toInt != 0 => NonNegativeIntegerCell((i1.toInt / i2.toInt).toString)
            case "*" => NonNegativeIntegerCell((i1.toInt * i2.toInt).toString)
            case _ => WrongFormatCell(WrongFormatMessage)
          }
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
      val MatrixIterator = stringMatrixIterator(processedSpreadsheet)
      val fileWriter = new FileWriter(new File(writePath)) // try catch
      val resultText = buildText(MatrixIterator, "")
      fileWriter.write(resultText)
      fileWriter.close()
      Right(processedSpreadsheet)
    }

    def stringMatrixIterator(processedSpreadsheet: ProcessedSpreadsheet): Iterator[List[String]] = {
      val resultMatrix = processedSpreadsheet.matrix.map(x => x.map(y => y.value))
      resultMatrix.iterator
    }

    def buildText(iterator: Iterator[List[String]], initStr: String): String = {
      if (iterator.hasNext) {
        val line = initStr + iterator.next().reduceLeft((x, y) => x + "\t" + y) + System.getProperty("line.separator") //last empty string
        buildText(iterator, line)
      } else initStr
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
