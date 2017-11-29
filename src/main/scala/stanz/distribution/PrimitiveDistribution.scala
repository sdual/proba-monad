package stanz.distribution

import scala.util.Random

trait PrimitiveDistribution[A] extends ProbabilityMonad[A] {

  def sample(random: Random): A

}
