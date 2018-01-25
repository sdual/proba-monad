package stanz.typeclass

trait Monad[M[_]] {

  def bind[A, B](f: A => M[B]): M[B]

  def flatMap[A, B](f: A => M[B]): M[B] = bind(f)

}
