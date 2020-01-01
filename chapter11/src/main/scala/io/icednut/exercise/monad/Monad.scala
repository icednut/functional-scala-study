package io.icednut.exercise.monad

import io.icednut.exercise.sources.laziness.Stream
import io.icednut.exercise.sources.parsing.instances.ReferenceTypes.Parser
import io.icednut.exercise.sources.testing.Gen

/**
 * @author will.109
 * @date 2019/12/09
 **/
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))


  // 모나드 조합기 함수들
  def sequence[A](lma: List[F[A]]): F[List[A]] = ???

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = ???

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
}

object Monad {

  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa flatMap f
  }

  val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = ???

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa flatMap f
  }
}