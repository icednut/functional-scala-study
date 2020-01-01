package io.icednut.exercise.monad

import io.icednut.exercise.monad.StateMonad.IntState

/**
 * @author will.109
 * @date 2019/12/10
 **/
case class State[S, A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object StateMonad {
  type IntState[A] = State[Int, A]

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
  }

  val F = stateMonad[Int]

//  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
//    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
//      xs <- acc
//      n <- getState
//      _ <- setState(n + 1)
//    } yield (n, a) :: xs).run(0)._1.reverse
}

object IntStateMonad extends Monad[IntState] {
  override def unit[A](a: => A): IntState[A] = State(s => (a, s))

  override def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = fa flatMap f
}