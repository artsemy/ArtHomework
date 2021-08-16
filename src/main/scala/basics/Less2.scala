package basics

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn
import java.io.{File, FileWriter}

object Less2 extends App {

  sealed trait CellContainer {
    def getValue: String
  }

  object CellContainer {

    final case class NonNegativeInteger(i: Int) extends CellContainer {
      override def getValue: String = i.toString
    }
    final case class TextLine(textLine: String) extends CellContainer {
      override def getValue: String = textLine
    }
    final case class EmptyCell(emptyLine: String) extends CellContainer {
      override def getValue: String = emptyLine
    }
    final case class CellLink(link: String) extends CellContainer {
      override def getValue: String = link
    }
    final case class WrongFormat(line: String) extends CellContainer {
      override def getValue: String = line
    }
    final case class Expression(expression: String) extends CellContainer {
      override def getValue: String = expression
    }

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

  final case class MyCounter(matrix: Map[String, CellContainer]) {
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
      else {
        signs.next()
        units.reduceLeft((x, y) => countPair(x, y, signs.next()))
      }
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
//      "D:\\input.txt"
    }

    def generateMatrix(iterator: Iterator[String]): Map[String, CellContainer] = {
      val startMatrix = Map.empty[String, CellContainer]
      parceLine(1, iterator, startMatrix)
    }

    @tailrec
    def parceLine(indexLine: Int, iterator: Iterator[String], matrix: Map[String, CellContainer]): Map[String, CellContainer] = {
      if (iterator.hasNext) {
        val line  = iterator.next().split("\t")
        val letters = 'A' to 'Z'
        val arr = for {
          i <- (0 until line.size).toList
        } yield (letters(i).toString.appendedAll(indexLine.toString), CellContainer(line(i)))
        val newMatrix = arr.toMap
        parceLine(indexLine+1, iterator, matrix ++ newMatrix)
      }
      else matrix
    }
  }

  final case class MyWriter() {
    def writeMatrix(matrix: Map[String, CellContainer]): Unit = {
      val fileWriter = new FileWriter(new File("D:\\output.txt"))
      val s = buildText(matrix)
      fileWriter.write(s)
      fileWriter.close()
    }


    def buildText(matrix: Map[String, CellContainer]): String = {
      val m = matrix.map(x => (x._1.tail+x._1.head, x._2)) //sorting problem
      val sortedSeq = m.toSeq.sortBy(_._1)
      val iter = sortedSeq.iterator
      buildLines(iter, iter.next()._2.getValue)
    }

    @tailrec
    def buildLines(iterator: Iterator[(String, CellContainer)], text: String): String = {
      if (iterator.hasNext) {
        val elem = iterator.next()
        if (elem._1.charAt(1) != 'A') buildLines(iterator, text + "\t" + elem._2.getValue)
        else buildLines(iterator, text + System.getProperty("line.separator") + elem._2.getValue)
      }
      else text
    }

  }

  val matrix = MyReader().readMatrix()
  val matrix2 = MyCounter(matrix).count()
  MyWriter().writeMatrix(matrix2)

}
