package chapter6

import cats.data.Validated.{Invalid, Valid}

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

  val x = List(Validated.invalid[String, String]("Fail1"),Validated.invalid[String, String]("Fail2"), Validated.valid[String, String]("Success1"))
    .foldLeft((List.empty[String], List.empty[String])){
    (valid, validated) => validated match {
      case Invalid(message) => (message :: valid._1, valid._2)
      case Valid(checkName) => (valid._1, checkName :: valid._2)
    }
  }

  println(x)

  val isItAllValid = List(Validated.invalid("Fail1"),Validated.invalid("Fail2")).forall(_.isValid)

  println(isItAllValid)
}
