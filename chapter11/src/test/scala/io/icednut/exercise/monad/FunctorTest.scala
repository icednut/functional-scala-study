package io.icednut.exercise.monad

import org.scalatest.FunSuite

/**
 * @author will.109
 * @date 2019/12/08
 **/
class FunctorTest extends FunSuite {

  test("listFunctor 인스턴스를 가지고 지퍼열기(unzip)을 해보자.") {
    val targetList = List((1, 2), (2, 3), (4, 10))
    val unzipedList = Functor.listFunctor.distribute(targetList)

    assert(unzipedList._1 == List(1, 2, 4))
    assert(unzipedList._2 == List(2, 3, 10))
  }

  test("List에 codistribute를 적용해보자.") {
    val target = Right(List(1, 2, 3))
    val codistributedList = Functor.listFunctor.codistribute(target)

    assert(codistributedList == List(Right(1), Right(2), Right(3)))
  }

  test("map(x)가 x의 구조를 보존하는지 한 번 직접 호출해서 살펴보자.") {
    val target = List(1, 2, 3)
    val mappedList = Functor.listFunctor.map(target)(_ * 2)

    assert(mappedList == List(2, 4, 6))
  }

  test("map(x)가 x의 구조를 보존하는지 한 번 직접 호출해서 살펴보자. 2") {
    val target = List(1, 2, 3)
    val mappedList = Functor.listFunctor.map(target)(element => element)

    assert(mappedList == target)
  }
}
