package stanz.distribution

import stanz.Probability

import scalaz.{Functor, Monad}

trait ProbabilityMonad[A]

object ProbabilityMonad {
  case class Point[A](value: A) extends ProbabilityMonad[A]
  case class FlatMap[A, B](dist: ProbabilityMonad[A], f: A => ProbabilityMonad[B]) extends ProbabilityMonad[B]
  case class Primitive[A](dist: PrimitiveDistribution[A]) extends ProbabilityMonad[A]
  case class Conditional[A](dist: ProbabilityMonad[A], likelihood: A => Probability) extends ProbabilityMonad[A]
}

sealed trait ProbabilityMonadInstances {

  import ProbabilityMonad._

  implicit val probabilityMonadInstances = new Functor[ProbabilityMonad] with Monad[ProbabilityMonad] {

    def point[A](a: => A): ProbabilityMonad[A] = Point(a)

    def bind[A, B](fa: ProbabilityMonad[A])(f: A => ProbabilityMonad[B]): ProbabilityMonad[B] = FlatMap(fa, f)

    override def map[A, B](fa: ProbabilityMonad[A])(f: A => B): ProbabilityMonad[B] = FlatMap(fa, (a: A) => Point(f(a)))

  }

}
