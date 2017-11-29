package stanz.sandbox.modeling.explicit

import scalaz.{Functor, Monad}

case class Explicit[A](list: List[(A, Double)])

trait ExplicitInstances {

  implicit val explicitInstance = new Functor[Explicit] with Monad[Explicit] {

    def point[A](a: => A): Explicit[A] = Explicit((a, 1.0) :: Nil)

    def bind[A, B](fa: Explicit[A])(f: A => Explicit[B]): Explicit[B] = {
      Explicit(
        for {
          (valueA, probA) <- fa.list
          (valueB, probB) <- f(valueA).list
        } yield (valueB, probA * probB)
      )
    }

    override def map[A, B](fa: Explicit[A])(f: A => B): Explicit[B] = {
      Explicit(fa.list.map {
        case (value, prob) => (f(value), prob)
      })
    }

  }

}

object ExplicitInstances extends ExplicitInstances

class Distribution {

  def reweight[A](factor: Double, explicit: Explicit[A]): Explicit[A] = {
    Explicit(explicit.list.map {
      case (value, prob) => (value, prob * factor)
    })
  }

  def normalize[A](explicit: Explicit[A]): Explicit[A] = {
    Explicit(
      for {
        (value, prob) <- explicit.list
        total = (explicit.list map {
          case (_, p) => p
        }).sum
      } yield (value, prob / total)
    )
  }

  def uniform[A](values: List[A]): Explicit[A] = {
    normalize {
      Explicit(
        values map {
          v => (v, 1.0)
        }
      )
    }
  }

  def categorical[A](prob: List[(A, Double)]): Explicit[A] = {
    normalize {
      Explicit(prob)
    }
  }

  def bernoulli(p: Double): Explicit[Boolean] = {
    categorical {
      List((true, p), (false, 1.0 - p))
    }
  }

}
