package chatper4

import chapter3.FunctorDemo.{Failure, Success, Warning}

object MonadDemo {
  import scala.language.higherKinds

  def flatMap[F[_], A, B](value: F[A])(f: A => F[B]): F[B] = ???
  def pure[F[_], A](value: A): F[A] = ???

  def map[F[_], A, B](value: F[A])(f: A => B): F[B] = {
    flatMap(value)(a => pure(f(a)))
  }
}

object ResultMonad {
  sealed trait Result[+A]

  // defined trait Result
  final case class Success[A](value: A) extends Result[A]

  final case class Warning[A](value: A, message: String) extends Result[A]

  final case class Failure(message: String) extends Result[Nothing]

  def success[A](value: A): Result[A] = Success(value)

  def warning[A](value: A, message: String): Result[A] = Warning(value, message)

  def failure[A](message: String): Result[A] = Failure(message)

  import cats.Monad

  val resultMonad = new Monad[Result] {
    override def pure[A](x: A): Result[A] = Success(x)

    override def flatMap[A, B](fa: Result[A])(f: (A) => Result[B]): Result[B] = {
      fa match {
        case Success(value) => f(value)
        case Warning(value, message1) => f(value) match {
          case Success(value) => Warning(value, message1)
          case Warning(value, message2) => Warning(value, s"$message1 $message2")
          case Failure(message) => Failure(message)
        }
        case Failure(message) => Failure(message)
      }
    }

    override def tailRecM[A, B](a: A)(f: (A) => Result[Either[A, B]]): Result[B] = ???
  }
}

class MonadDemo {

}
