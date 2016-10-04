package chapter2

object CatMonoid extends App {

  import cats.syntax.semigroup._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.option._

  import cats.Monoid

  def add[T](items: List[T])(implicit monoidForT: Monoid[T]): T = {
    items.foldLeft(monoidForT.empty)(monoidForT.combine)
  }

  println(add[Int](List(1,2,3)))
  println(add[Option[Int]](List(Some(1),Some(2),None)))
}
