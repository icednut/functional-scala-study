package io.icednut.scala.study

/**
 * @author will.109
 * @date 14/10/2019
 **/
sealed trait MyEither[+E, +A] {

  // 연습문제 4.6
  def map[B](f: A => B): MyEither[E, B] = {
    this match {
      case MyRight(v) => MyRight(f(v))
      case MyLeft(e) => MyLeft(e)
    }
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyRight(v) => f(v)
      case MyLeft(e) => MyLeft(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyRight(v) => MyRight(v)
      case MyLeft(e) => b
    }
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    this match {
      case MyRight(v) => b.flatMap(bv => MyRight(f(v, bv)))
      case MyLeft(e) => MyLeft(e)
    }
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]


object EitherExercise {

  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
    if (xs == null || xs.isEmpty) MyLeft("Mean of empty list!")
    else MyRight(xs.sum / xs.length)

  def Try[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch {
      case e: Exception => MyLeft(e)
    }

  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] =
    try MyRight(x / y)
    catch {
      case e: Exception => MyLeft(e)
    }

  // 연습문제 4.7
  //  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = {
  //    val value: (E, List[A]) = es.foldRight((Nil.asInstanceOf[E], List[A]()))((element, result) => {
  //      element match {
  //        case MyRight(v) => (result._1, result._2.::(v))
  //        case MyLeft(e) => (e, result._2)
  //      }
  //    })
  //
  //    value match {
  //      case (Nil, results: List[A]) => MyRight(results)
  //      case (e, _) => MyLeft(e)
  //    }
  //  }

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
    val es: List[MyEither[E, B]] = as.map(v => f(v))

    val value: (E, List[B]) = es.foldRight((Nil.asInstanceOf[E], List[B]()))((element, result) => {
      element match {
        case MyRight(v) => (result._1, result._2.::(v))
        case MyLeft(e) => (e, result._2)
      }
    })

    value match {
      case (Nil, results: List[B]) => MyRight(results)
      case (e, _) => MyLeft(e)
    }
  }
}