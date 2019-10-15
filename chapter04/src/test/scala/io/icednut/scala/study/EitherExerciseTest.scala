package io.icednut.scala.study

import io.icednut.scala.study.EitherExercise.{_}
import org.scalatest.FunSuite

/**
 * @author will.109
 * @date 14/10/2019
 **/
class EitherExerciseTest extends FunSuite {

  test("Either의 성공 사례") {
    val result = mean(IndexedSeq(1.0, 2.0, 3.0, 4.0))

    assert(result == MyRight(2.5))
  }

  test("Either의 실패 사례") {
    val result = mean(null)

    assert(result.isInstanceOf[MyLeft[String]])
  }

  // 연습문제 4.6
  test("map 함수를 구현해야 한다.") {
    val value = MyRight(1)
    val mappedValue = value.map("Hello " + _)

    assert(mappedValue.leftSide == MyRight("Hello 1"))
  }

  test("map 함수를 구현해야 한다. 검증하기") {
    val value = MyRight(1)
    val mappedValue = value.map(safeDiv(_, 0))

    print(mappedValue)
  }

  test("flatMap을 구현해야 한다. Left일 때 flatmap 검증을 해보자.") {
    val value2 = MyRight(0) // MyEither[Nothing, Int]
    val flatmappedValue2 = value2.flatMap[Exception, Int](v => safeDiv(1, v.toInt))

    print(flatmappedValue2)
    assert(flatmappedValue2.isInstanceOf[MyLeft[String]])
  }

  test("flatMap을 구현해야 한다. Left일 때 flatmap 검증을 해보자. 2") {
    val value2 = MyRight("Hello, world") // MyEither[Nothing, String]
    val flatmappedValue = value2.flatMap(v => Try {
      v.toInt
    }) // MyEither[Exception, String]

    print(flatmappedValue)
    assert(flatmappedValue.isInstanceOf[MyLeft[String]])
  }

  test("orElse를 구현해야 한다. 검증도 해보자.") {
    val value = MyRight("Hello, world!!")
    val actualVaule = value.orElse(MyLeft(20301))

    print(actualVaule)
    assert(actualVaule == MyRight("Hello, world!!"))
  }

  test("orElse를 구현해야 한다. 검증도 해보자. 2") {
    val value = MyLeft(new RuntimeException("뭔가 오류가 발생했습니다!!"))
    val actualVaule = value.orElse(MyLeft(20301)) // 오류 코드 반환

    print(actualVaule)
    assert(actualVaule == MyLeft(20301))
  }

  test("Either 2개를 합성하는 map2를 구현해야 한다.") {
    val value1 = MyRight(1)
    val value2 = MyRight(2)
    val result = value1.map2(value2)(_ + _)

    assert(result == MyRight(3))
  }

  test("Either 2개를 합성하는 map2를 구현해야 한다. 2") {
    val value1 = MyRight(1)
    val exception = new RuntimeException("으앙!")
    val value2 = MyLeft(exception)
    val result = value1.map2[Exception, Int, Int](value2)((v1, v2) => v1 + v2)

    assert(result == MyLeft(exception))
  }

  // 연습문제 4.7
  test("sequence 함수를 구현해야 한다. Left 검증하기") {
    val exception = new RuntimeException("아오!!")
    val es = List(MyRight(1), MyRight(2), MyLeft(exception), MyRight(4))
    val result = sequence(es)

    print(result)
    assert(result == MyLeft(exception))
  }

  test("sequence 함수를 구현해야 한다. Right 검증하기") {
    val es = List(MyRight(1), MyRight(2), MyRight(3), MyRight(4))
    val result = sequence(es)

    print(result)
    assert(result == MyRight(List(1, 2, 3, 4)))
  }
}
