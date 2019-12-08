package io.icednut.scala.exercise.util

/**
 * @author will.109
 * @date 13/11/2019
 **/
case class Precision(p: Double)

class withAlmostEquals(d: Double) {
  def ~=(d2: Double)(implicit p: Precision) = (d - d2).abs <= p.p
}
