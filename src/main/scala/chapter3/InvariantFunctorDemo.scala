package chapter3

import scala.util.Try

trait Codec[A] {
  self =>
  def encode(value: A): String

  def decode(value: String): Option[A]

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B) = self.encode(enc(value))

    override def decode(value: String) = self.decode(value).map(dec)

  }
}

case class Box[A](value: A)

object InvariantFunctorDemo extends App {

  implicit val intCodec: Codec[Int] = new Codec[Int] {
    override def encode(value: Int) = value.toString

    override def decode(value: String) = Try(value.toInt).toOption
  }


  println(s"decoding 1: ${intCodec.decode("1")}")
  println(s"decoding bad integer: ${intCodec.decode("bad integer")}")

  def boxCodec[A](implicit codecA: Codec[A]): Codec[Box[A]] = codecA.imap(Box(_), _.value)

  println(s"encode Box(123) to 123 : ${boxCodec.encode(Box[Int](123))}")
  println(s"decode 123 to Box(123): ${boxCodec.decode("123")}")



  //This demonstrate, if you have a Show[String], and you have a function to do (Symbol) => String
  //Then you can use contramap to get a Show[Symbol] from Show String

  import cats.Show
  import cats.functor.Contravariant
  import cats.instances.string._

  val showString = Show[String]

  val showSymbol: Show[Symbol] = Contravariant[Show].
    contramap(showString)((sym: Symbol) => s"'${sym.name}")

  println(s"show symbol: ${showSymbol.show('david)}")
}
