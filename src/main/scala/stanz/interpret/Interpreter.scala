package stanz.interpret

import scala.util.Random

trait Interpreter[A] {

  def sample(random: Random): A

}
