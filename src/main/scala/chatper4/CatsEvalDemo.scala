package chatper4

import cats.{Eval, Monad}
import cats.syntax.flatMap._

import scala.language.higherKinds
import cats.instances.option._
import cats.syntax.option._

object CatsEvalDemo extends App {

  def stackDepth: Int = Thread.currentThread.getStackTrace.length

  def loopM[M[_] : Monad](m: M[Int], count: Int): M[Int] = {
    println(s"Stack depth $stackDepth")
    count match {
      case 0 => m
      case n => m.flatMap { _ => loopM(m, n - 1) }
    }

  }

  loopM(1.some, 5)
  loopM(Eval.now(1), 5).value
}
