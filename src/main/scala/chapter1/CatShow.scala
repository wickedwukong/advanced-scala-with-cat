package chapter1

import cats.Show

object CatShow extends App {
  import cats.instances.int._

  val showInt = Show.apply[Int]

  println(showInt.show(1))

  import cats.syntax.show._
  val shownInt123 = 123.show
  println(shownInt123)
}
