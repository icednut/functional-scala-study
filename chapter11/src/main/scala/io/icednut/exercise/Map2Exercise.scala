package io.icednut.exercise

import io.icednut.exercise.sources.parsing.instances.ReferenceTypes.Parser
import io.icednut.exercise.sources.testing.Gen

/**
 * @author will.109
 * @date 2019/12/09
 **/
object Map2Exercise {

  def map2[A,B,C](fa: Gen[A], fb: Gen[B])(f: (A,B) => C): Gen[C] =
  fa flatMap (a => fb map (b => f(a,b)))

  def map2[A,B,C](fa: Parser[A], fb: Parser[B])(f: (A,B) => C): Parser[C] = ???
//    fa flatMap (a => fb map (b => f(a,b)))

  def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fa flatMap (a => fb map (b => f(a,b)))
}
