package chapter1

import cats.Show

object CatShow extends App {
  import cats.instances.int._

  val showInt = Show.apply[Int]

  println(showInt.show(1))
}
