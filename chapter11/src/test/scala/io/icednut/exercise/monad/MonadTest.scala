package io.icednut.exercise.monad

import io.icednut.exercise.sources.laziness
import org.scalatest.FunSuite

/**
 * @author will.109
 * @date 2019/12/09
 **/
class MonadTest extends FunSuite {

  test("[연습문제 11.1] Option 모나드로 map 연산을 할 수 있어야 한다.") {
    val target = Option(2)
    val result = Monad.optionMonad.map(target)(_ * 2)

    assert(result == Option(4))
  }


  test("[연습문제 11.1] Stream 모나드로 map 연산을 할 수 있어야 한다.") {
    val target: laziness.Stream[Int] = laziness.Stream(1, 2)
    val result = Monad.streamMonad.map(target)(_ * 2)

    assert(result.toList == List(2, 4))
  }


  test("[연습문제 11.1] List 모나드로 map 연산을 할 수 있어야 한다.") {
    val target: List[Int] = List(1, 2)
    val result = Monad.listMonad.map(target)(_ * 2)

    assert(result == List(2, 4))
  }

  test("[연습문제 11.7] compose 구현한거를 검증해보자.") {
    val f: Int => List[Int] = (a: Int) => List(a * 2)
    val g: Int => List[Int] = (b: Int) => List(b - 1)
    val composedMonad = Monad.listMonad.compose(f, g)
    val result = composedMonad(10)

    assert(result == List(19))
  }

  test("[연습문제 11.7] compose로 모나드 결합법칙을 검증해보자.") {
    val f: Int => List[Int] = (a: Int) => List(a * 2)
    val g: Int => List[Int] = (b: Int) => List(b - 1)
    val h: Int => List[Int] = (c: Int) => List(c, c * 2)
    val composedMonad1 = Monad.listMonad.compose(Monad.listMonad.compose(f, g), h)
    val composedMonad2 = Monad.listMonad.compose(f, Monad.listMonad.compose(g, h))

    assert(composedMonad1(10) == composedMonad2(10))
  }

  test("[연습문제 11.11] 모나드 하나를 선택해서 그 모나드에 대해 항등법칙이 성립함을 증명") {
    val f: Int => List[Int] = (a: Int) => List(a * 2)
    val unit: Int => List[Int] = (b: Int) => Monad.listMonad.unit(b)
    val composedMonad1 = Monad.listMonad.compose(f, unit)
    val composedMonad2 = Monad.listMonad.compose(unit, f)

    assert(composedMonad1(10) == composedMonad2(10))
  }

  test("[연습문제 11.10] 항등법칙의 이 두 표현이 동치임을 증명하라.") {
    val unit: Int => List[Int] = (b: Int) => Monad.listMonad.unit(b)
    val f: Int => List[Int] = (a: Int) => List(a)

    val monad1 = Monad.listMonad.flatMap(List(10))(unit)
    val monad2 = Monad.listMonad.flatMap(unit(10))(f)

    assert(monad1 == List(10))
    assert(monad2 == f(10))
    assert(monad1 == monad2)
  }
}
