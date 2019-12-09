package io.icednut.exercise

import io.icednut.exercise.sources.parsing.instances.ReferenceTypes.Parser
import io.icednut.exercise.sources.testing.Gen
import io.icednut.exercise.sources.errorhandling.Option

object MapExercise {

  def map[A, B](ga: Gen[A])(f: A => B): Gen[B] = ???

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = ???

  def map[A, B](oa: Option[A])(f: A => B): Option[B] = ???
}
