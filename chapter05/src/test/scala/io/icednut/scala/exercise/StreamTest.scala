package io.icednut.scala.exercise

import io.icednut.scala.exercise.util.{Precision, withAlmostEquals}
import org.scalatest.{FunSuite, Matchers}

/**
 * @author will.109
 * @date 04/11/2019
 **/
class StreamTest extends FunSuite with Matchers {

  test("[연습문제 5.1] toList를 구현해야 한다.") {
    Stream("a", "b", "c").toList() should be(List("a", "b", "c"))
  }

  test("[연습문제 5.2] 처음 n개의 요소를 반환하는 take(n)을 구현해야 한다.") {
    val myTurn = Stream("a", "b", "c").take(2).toList()
    val given = Stream("a", "b").toList()

    myTurn should be(given)
  }

  test("[연습문제 5.2] 처음 n개의 요소를 건너뛰어 반환하는 drop(n)을 구현해야 한다.") {
    val myTurn = Stream("a", "b", "c").drop(2).toList()
    val given = Stream("c").toList()

    myTurn should be(given)
  }

  test("[연습문제 5.3] takeWhile을 구현해야 한다.") {
    val myTurn = Stream(1, 2, 3, 4, 5, 6).takeWhile(_ % 2 == 0).toList()
    val given = Stream(2, 4, 6).toList()

    myTurn should be(given)
  }

  test("[연습문제 5.4] forAl을 구현해야 한다.") {
  }

  test("[연습문제 5.5] takeWhile을 구현하라.") {
  }

  test("[연습문제 5.6] foldRight을 이용하여 headOption을 구현하라.") {
  }

  test("[연습문제 5.7] foldRight을 이용하여 map을 구현하라.") {
  }

  test("[연습문제 5.7] foldRight을 이용하여 filter을 구현하라.") {
  }

  test("[연습문제 5.7] foldRight을 이용하여 append을 구현하라. append는 자신의 인수에 대해 엄격하지 않아야 한다.") {
  }

  test("[연습문제 5.7] flatMap을 이용하여 append을 구현하라.") {
  }

  implicit def add_~=(d:Double) = new withAlmostEquals(d)
  implicit val precision = Precision(0.001)
  implicit val ratio = 90.0

  test("0 ~ N 입력 시 90%에 해당하는 값 출력하기 2") {
    (PercentageCalculator.getPercentage(2) ~= 81.0) should be(true)
  }

  test("0 ~ N 입력 시 90%에 해당하는 값 출력하기 0") {
    (PercentageCalculator.getPercentage(0) ~= 100.0) should be(true)
  }

  test("0 ~ N 입력 시 90%에 해당하는 값 출력하기 10") {
    (PercentageCalculator.getPercentage(10) ~= 34.867844) should be(true)
  }

  test("0 ~ N 입력 시 90%에 해당하는 값 출력하기 16") {
    (PercentageCalculator.getPercentage(16) ~= 18.5302019) should be(true)
  }
}
