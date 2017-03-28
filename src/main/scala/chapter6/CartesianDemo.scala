package chapter6

object CartesianDemo extends App {

  import cats.Monoid
  import cats.instances.boolean._
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.cartesian._

  case class AnotherCat(name: String, born: Int, color: String)

  (Option("Garfield") |@| Option(1978) |@| Option("Orange and black")).map(AnotherCat.apply)


  case class Cat(
                  name: String,
                  yearOfBirth: Int,
                  favoriteFoods: List[String]
                )

  def catToTuple(cat: Cat) =
    (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String] |@|
      Monoid[Int] |@|
      Monoid[List[String]]
    ).imap(Cat.apply)(catToTuple)


  import cats.syntax.monoid._

  val emptyCat: Cat = Monoid[Cat].empty

  println(emptyCat)


  val garfield   = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

  val twoCats: Cat = garfield |+| heathcliff

  println(twoCats)
}
