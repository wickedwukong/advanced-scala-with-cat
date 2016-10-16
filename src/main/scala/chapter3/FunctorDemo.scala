package chapter3

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object FunctorDemo {

  sealed trait Result[+A] // defined trait Result
  final case class Success[A](value: A) extends Result[A] // defined class Success
  final case class Warning[A](value: A, message: String) extends Result[A] // defined class Warning
  final case class Failure(message: String) extends Result[Nothing]

  val resultFunctor = new Functor[Result] {
    override def map[A, B](a: Result[A])(f: A => B): Result[B] = {
      a match {
        case Success(value) => Success(f(value))
        case Warning(value, message) => Warning(f(value), message)
        case Failure(message) => Failure(message)
      }
    }
  }
}
