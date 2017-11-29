package stanz.sandbox.modeling.algorithm

import stanz.sandbox.modeling.Probability
import stanz.sandbox.modeling.distribution.DistributionInstances._
import stanz.sandbox.modeling.distribution.{Distribution, Prior}

import scala.annotation.tailrec
import scalaz.Scalaz._

/**
  * Created by tenoritama on 1/15/17.
  */


class MetropolisHastings {

  def run[A](n: Int, d: Distribution[A]): Distribution[List[A]] = {

    val proposal: Distribution[(A, Probability)] = Prior.prior(d)

    @tailrec
    def iterate(i: Int, prob: Distribution[List[(A, Probability)]]): Distribution[List[(A, Probability)]] = {
      i match {
        case 0 => prob
        case _ =>
          val nextDist = for {
            p <- prob
            (v1, p1) = p.head
            prop <- proposal
            (v2, p2) = prop
            isAccepted <- Distribution.bernoulli(1.0 min p2 / p1)
            next = if (isAccepted) (v2, p2) else (v1, p1)
          } yield next :: p
          iterate(i - 1, nextDist)
      }
    }

    for {
      result <- iterate(n, proposal.map(x => List(x)))
    } yield result.map(x => x._1)

  }

}
