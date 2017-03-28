package chapter3

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self.format(func(value))
  }
}


object ContravariantFunctorDemo extends App {

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)



}
