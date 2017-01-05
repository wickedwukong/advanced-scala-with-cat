package chapter5

import cats.data.{Writer, Xor, XorT}
import cats.implicits._

import scala.concurrent.Await

object MonadTransformerDemo extends App {

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] = util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None => Writer(List(s"Failed on $str"), None)
  }

  def addNumbers(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    // Transform the incoming stacks to work on them:
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  val result1: Logged[Option[Int]] = addNumbers("1", "2", "3")

  println(result1)

}

object FutureXorMonadTransformerDemo extends App {

  import scala.concurrent.Future
  import cats.instances.future._
  import cats.syntax.flatMap._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._


  type FutureXor[A] = XorT[Future, String, A]


  val loadAverages = Map(
    "a.example.com" -> 0.1,
    "b.example.com" -> 0.5,
    "c.example.com" -> 0.2
  )

  def getLoad(hostname: String): FutureXor[Double] = {
    loadAverages.get(hostname) match {
      case Some(avg) => XorT.right(Future.successful(avg))
      case None => XorT.left(Future.successful(s"Host unreachable: $hostname"))
    }
  }

  def getMeanLoad(hostnames: List[String]): FutureXor[Double] = {
    import cats.instances.list._
    import cats.syntax.traverse._

    hostnames.length match {
      case 0 => XorT.left(Future.successful(s"No hosts to contact"))
      case n => hostnames.map(getLoad).sequence.map(_.sum / n)
    }
  }

  getMeanLoad(List("a.example.com"))


  def report[A](input: FutureXor[A]): Unit = {
    Await.result(input.value, 2.seconds).fold(
      msg => println("[FAIL] " + msg),
      ans => println("[DONE] " + ans)
    )
  }

  report(getMeanLoad(List("a.example.com", "b.example.com")))
}

