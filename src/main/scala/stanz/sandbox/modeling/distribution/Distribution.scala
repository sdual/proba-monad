package stanz.sandbox.modeling.distribution

import stanz.sandbox.modeling.Probability

import scala.util.Random
import scalaz.{Functor, Monad}
import stanz.sandbox.modeling.distribution.DistributionInstances._

import scala.annotation.tailrec
import scalaz.Scalaz._

sealed trait Distribution[A] {

  def sample(random: Random): A = {

    @tailrec
    def loop(dist: Distribution[A], random: Random): A = {
      dist match {
        case pt1: Point[A]      => pt1.value
        case fm1: FlatMap[A, _] => fm1.dist match {
          case pt2: Point[A]      => loop(fm1.f(pt2.value), random)
          case fm2: FlatMap[A, _] => loop(fm2.dist flatMap (a => fm2.f(a) flatMap fm1.f), random)
          case pr2: Primitive[A]  => loop(fm1.f(pr2.fa.sample(random)), random)
        }
        case pr1: Primitive[A] => pr1.fa.sample(random)
        case _ => throw new Exception("can't sample.")
      }
    }

    loop(this, random)
  }
}

sealed trait DistributionInstances {

  implicit val distributionInstance = new Functor[Distribution] with Monad[Distribution] {

    def point[A](a: => A): Distribution[A] = Point(a)

    def bind[A, B](fa: Distribution[A])(f: A => Distribution[B]): Distribution[B] = FlatMap(fa, f)

    override def map[A, B](fa: Distribution[A])(f: A => B): Distribution[B] = FlatMap(fa, (a: A) => Point(f(a)))

    //def conditional[A](fa: Distribution[A])(likelihood: A => Probability): Distribution[A] = Conditional(fa, likelihood)

  }

}

object DistributionInstances extends DistributionInstances

case class Point[A](value: A) extends Distribution[A]
case class FlatMap[A, B](dist: Distribution[A], f: A => Distribution[B]) extends Distribution[B]
case class Primitive[A](fa: Sampleable[A]) extends Distribution[A]
case class Conditional[A](dist: Distribution[A], likelihood: A => Probability) extends Distribution[A]

object Distribution {

  def bernoulli(prob: Probability): Distribution[Boolean] = {
    val bernoulli = new Bernoulli(prob)
    Primitive(bernoulli)
  }

}

sealed trait Sampleable[A] {
  def sample(random: Random): A
}

class Bernoulli(prob: Probability) extends Sampleable[Boolean] {
  def sample(random: Random): Boolean = {
    prob > random.nextDouble()
  }
}

class Normal(mean: Double, stdDev: Double) extends Sampleable[Double] {
  def sample(random: Random): Double = {
    val sampled = random.nextGaussian()
    (stdDev * sampled) + mean
  }
}
