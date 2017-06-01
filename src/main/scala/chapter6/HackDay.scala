package chapter6

object HackDay {

  import cats.syntax.either._

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(_ => s"Couldn't read $str")

  for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield (a + b + c)



  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val timestamp0 = System.currentTimeMillis

  def getTimestamp: Long = {
    val timestamp = System.currentTimeMillis - timestamp0
    Thread.sleep(100)
    timestamp
  }

  val timestamps = for {
    a <- Future(getTimestamp)
    b <- Future(getTimestamp)
    c <- Future(getTimestamp)
  } yield (a, b, c)

  Await.result(timestamps, 1.second)

}
