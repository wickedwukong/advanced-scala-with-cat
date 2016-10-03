package chapter1

object CatEqDemo extends App {

  import cats.Eq
  import cats.instances.int._

  val eqInt = Eq[Int]

  println(eqInt.eqv(123, 123))
  println(eqInt.eqv(123, 234))

  import cats.syntax.eq._

  123 === 123

  123 =!= 234
}

object CatEq extends App {

  final case class Cat(name: String, age: Int, owner: Option[String])

  import cats.Eq
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._

  import cats.syntax.eq._

  implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) && (cat1.age === cat2.age) && (cat1.owner === cat2.owner)
  }

  val cat1 = Cat("Garfield", 35, Option("orange and black"))
  val cat2 = Cat("Heathcliff", 30, Option("orange and black"))

  println(cat1 === cat2)

}
