package io.icednut.scala.exercise

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
}
