package chapter3

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self.format(func(value))
  }
}


object ContravariantFunctorDemo extends App {

  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  println(s"format hello: ${format("hello")}")
  println(s"format true: ${format(true)}")

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] = printable.contramap(box => box.value)

  println(s"format Box hello: ${format(Box("hello"))}")
  println(s"format Box true: ${format(Box(true))}")
}
