package stanz.sandbox.modeling.distribution

import stanz.sandbox.modeling.Probability
import stanz.sandbox.modeling.distribution.DistributionInstances._

import scalaz.Scalaz._
import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz.Trampoline

//sealed trait Trampoline[+A] {
//  @tailrec
//  final def runT: A = resume match {
//    case Right(a) => a
//    case Left(k) => k().runT
//  }
//
//  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
//    this match {
//      case Trampoline.FlatMap(a, g) => Trampoline.FlatMap(a, (x: Any) => g(x) flatMap f)
//      case x => Trampoline.FlatMap(x, f)
//    }
//  }
//
//  def map[B](f: A => B): Trampoline[B] = {
//    flatMap(a => Trampoline.Done(f(a)))
//  }
//
//  @tailrec
//  final def resume: Either[() => Trampoline[A], A] =
//    this match {
//      case Trampoline.Done(a) => Right(a)
//      case Trampoline.More(k) => Left(k)
//      case Trampoline.FlatMap(a, f) => a match {
//        case Trampoline.Done(a) => f(a).resume
//        case Trampoline.More(k) => Left(() => k() flatMap f)
//        case Trampoline.FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
//      }
//    }
//
//}
//
//object Trampoline {
//  case class Done[A](a: A) extends Trampoline[A]
//  case class More[A](k: () => Trampoline[A]) extends Trampoline[A]
//  case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
//}

object Prior {

  def prior[A](dist: Distribution[A]): Distribution[(A, Probability)] = {

    def loop(dist: Distribution[A]): Trampoline[Distribution[(A, Probability)]] = {
      dist match {
        case cond: Conditional[A] =>
          Trampoline.suspend(loop(cond.dist).map(l => l.map(vp => (vp._1, vp._2 * cond.likelihood(vp._1)))))
        case fm: FlatMap[A, _] =>
          Trampoline.suspend(loop(fm.dist).map(l => l.flatMap(vp => fm.f(vp._1).map(y => (y, vp._2)))))
        case otherwise =>
          Trampoline.done(otherwise.map(v => (v, 1.0)))
      }
    }
    loop(dist).run
  }

}
