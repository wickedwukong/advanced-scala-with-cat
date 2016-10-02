package chapter1

trait Printable[A] {
  def format(a: A): String
}


object PrintDefaults {
  implicit val intPrintable = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }

  implicit val stringPrintable = new Printable[String] {
    override def format(a: String): String = a
  }
}

object Print {
  def format[A](a: A)(implicit printable: Printable[A]) = printable.format(a)
  def print[A](input: A)(implicit printer: Printable[A]): Unit = { println(format(input))}
}

final case class Cat(name: String, age: Int, color: String)

object Cat {
  import PrintDefaults._
  implicit val catPrintable = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name = Print.format(cat.name)
      val age = Print.format(cat.age)
      val color = Print.format(cat.color)

      s"$name is a $age year-old $color cat."
    }
  }

}

object PrintSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(printable.format(value))
  }
}

object Main extends App {
  import Print._
  import PrintDefaults._
  import Print._
  import PrintSyntax._

  Print.print(1)

   val cat = Cat("Mason", 100, "black")

  print(cat)

  cat.print
}