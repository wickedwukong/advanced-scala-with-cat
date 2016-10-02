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

object Test extends App {
  import Print._
  import PrintDefaults._
  import Print._

  Print.print(1)
}