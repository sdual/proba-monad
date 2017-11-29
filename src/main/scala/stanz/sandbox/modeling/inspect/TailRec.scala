package stanz.sandbox.modeling.inspect

sealed trait TailRec[A] {
  final def run : A = this match {
    case Return(v)  => v
    case Suspend(k) => k().run
  }
}

case class Return[A](v : A) extends TailRec[A]
case class Suspend[A](resume : () => TailRec[A]) extends TailRec[A]
