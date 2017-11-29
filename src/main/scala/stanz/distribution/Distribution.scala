package stanz.distribution

import stanz.interpret.Interpreter

import scala.util.Random

trait Distribution[A] extends ProbabilityMonad[A] {

  val interpreter: Interpreter[A]

  def sample(random: Random): A = {
    interpreter.sample(random)
  }

}
