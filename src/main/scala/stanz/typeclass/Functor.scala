package stanz.typeclass

trait Functor[F[_]] {

  def map[A, B](f: A => B): F[B]

}
