package stanz.typeclass

trait Applicative[F[_]] {

  def point[A](a: => A): F[A]

  def pure[A](a: => A): F[A] = point(a)

}
