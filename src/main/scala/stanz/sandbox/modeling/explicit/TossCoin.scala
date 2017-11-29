package stanz.sandbox.modeling.explicit

import stanz.sandbox.modeling.explicit.ExplicitInstances._
import scalaz.Scalaz._

sealed trait Coin
case object Heads extends Coin
case object Tails extends Coin

object TossCoin extends App {

  def coin: Explicit[Coin] = Explicit(Heads -> 0.5 :: Tails -> 0.5 :: Nil)
  def loadedCoin: Explicit[Coin] = Explicit(Heads -> 0.1 :: Tails -> 0.9 :: Nil)

  val result = for {
    a <- coin
    b <- coin
    c <- loadedCoin
  } yield {a == Tails && b == Tails && c == Tails }

  println(result)
}
