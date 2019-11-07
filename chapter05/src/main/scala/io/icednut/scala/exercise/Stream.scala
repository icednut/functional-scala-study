package io.icednut.scala.exercise

/**
 * @author will.109
 * @date 04/11/2019
 **/
sealed trait Stream[+A] {

  def toList(): List[A] = {
    def go(next: Stream[A]): List[A] = {
      next match {
        case Cons(h, t) => h() :: go(t())
        case Empty => List()
      }
    }

    go(this)
  }

  def take(value: Int): Stream[A] = {
    def go(nextTake: Int, nextStream: Stream[A]): Stream[A] = {
      nextStream match {
        case Cons(h, t) => {
          val newNextTake = nextTake - 1
          val newHead = h()
          val newTail = if (newNextTake <= 0) Empty else go(newNextTake, t())
          Stream.cons(newHead, newTail)
        }
        case Empty => nextStream
      }
    }

    go(value, this)
  }

  def drop(value: Int): Stream[A] = {
    ???
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}