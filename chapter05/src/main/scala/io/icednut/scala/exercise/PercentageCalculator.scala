package io.icednut.scala.exercise

import scala.annotation.tailrec

/**
 * @author will.109
 * @date 13/11/2019
 **/
object PercentageCalculator {

  def getPercentage(count: Integer)(implicit ratio: Double): Double = {

    @tailrec
    def go(nextCount: Integer, nextPercentage: Double): Double =
      if (nextCount > 0) {
        go(nextCount - 1, (ratio / 100) * nextPercentage)
      } else {
        nextPercentage
      }

    go(count, 100.0)
  }
}
