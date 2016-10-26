package chatper4

object MonadDemo {
  import scala.language.higherKinds

  def flatMap[F[_], A, B](value: F[A])(f: A => F[B]): F[B] = ???
  def pure[F[_], A](value: A): F[A] = ???

  def map[F[_], A, B](value: F[A])(f: A => B): F[B] = {
    flatMap(value)(a => pure(f(a)))
  }
}

class MonadDemo {

}
