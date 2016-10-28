package chapter6

object ValidatedDemo extends App {
  import cats.data.Validated
  import cats.instances.list._
  import cats.instances.string.catsKernelStdMonoidForString
  import cats.syntax.cartesian._

  val listOfValidated = (Validated.invalid(List("Fail1")) |@| Validated.invalid(List("Fail2"))
    ).tupled

  println(listOfValidated)


  val twoValidatedString = (Validated.invalid("Fail1") |@| Validated.invalid("Fail2"))

  val threeValidedString = (twoValidatedString |@| Validated.invalid("Fail3"))


  println(threeValidedString.tupled)

  val isItAllValid = List(Validated.invalid("Fail1"),Validated.invalid("Fail2")).forall(_.isValid)

  println(isItAllValid)
}
