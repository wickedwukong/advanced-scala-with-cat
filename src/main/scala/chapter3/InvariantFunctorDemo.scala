package chapter3

trait Codec[A] {
  self =>
  def encode(value: A): String
  def decode(value: String): Option[A]

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B) = self.encode(enc(value))

    override def decode(value: String) = self.decode(value).map(dec)

  }
}

object InvariantFunctorDemo extends App{

}
