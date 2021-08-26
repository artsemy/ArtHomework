package adt

import adt.Less5.RouletteColour
import adt.Less5.RouletteColour.{Black, Red}

object BetUtil {
  def isRedBetWin(rouletteColour: RouletteColour): Boolean = {
    rouletteColour == Red
  }
  def isBlackBetWin(rouletteColour: RouletteColour): Boolean = {
    rouletteColour == Black
  }
  def isEvenBetWin(rouletteNum: Int): Boolean = {
    rouletteNum % 2 == 0 && rouletteNum != 0
  }
  def isHalfBetWin(halfNUmber: Int, rouletteNum: Int): Boolean = {
    if (halfNUmber == 1) rouletteNum < 19
    else rouletteNum > 18 //halfNUmber == 2
  }
  def isDozeBetWin(dozenNumber: Int, rouletteNum: Int): Boolean = {
    if (dozenNumber == 1) rouletteNum < 13 //&& rouletteNum.number > 0
    else if (dozenNumber == 2) rouletteNum > 12 && rouletteNum < 25
    else rouletteNum > 24 //&& roulette.number < 37
  }
  def isColumnBetWin(columnNumber: Int, rouletteNum: Int): Boolean = {
    if (columnNumber == 1) rouletteNum % 3 == 1
    else if (columnNumber == 2) rouletteNum % 3 == 2
    else rouletteNum % 3 == 0 //columnNumber == 3
  }
  def isLineBetWin(lineNumber: Int, rouletteNum: Int): Boolean = {
    rouletteNum >= lineNumber && rouletteNum <= lineNumber + 6
  }
  def isCornerBetWin(cornerNumber: Int, rouletteNumber: Int): Boolean = {
    cornerNumber == rouletteNumber || cornerNumber + 1 == rouletteNumber ||
      cornerNumber + 3 == rouletteNumber || cornerNumber + 4 == rouletteNumber
  }
  def isStreetBetWin(streetNumber: Int, rouletteNumber: Int): Boolean = {
    rouletteNumber == streetNumber || rouletteNumber == streetNumber + 1 ||
      rouletteNumber == streetNumber + 2
  }
  def isSplitBetWin(splitNumber: Int, rouletteNumber: Int): Boolean = {
    rouletteNumber == splitNumber || rouletteNumber == splitNumber + 1
  }
  def isStraightUpBetWin(straightNumber: Int, rouletteNumber: Int): Boolean = {
    straightNumber == rouletteNumber
  }
}
