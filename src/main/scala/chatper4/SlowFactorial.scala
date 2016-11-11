package chatper4

import cats.Id
import cats.data.{Writer, WriterT}

import scala.concurrent.duration.Duration

object SlowFactorial {

  def slowly[T](f: => T) = {
    try f finally Thread.sleep(100)
  }

  def factorial(n: Int): Int = {
    val result = slowly(if (n == 0) 1 else n * factorial(n - 1))

    println(s"n is: $n result is: $result")

    result
  }

}

object WriterMonadDemoApp extends App {
  import SlowFactorial._
  println(factorial(5))
}

object WriterMonadDemoInterleavingOutputApp extends App {
  import SlowFactorial._
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global

  var futureFactorials: Future[List[Int]] = Future.sequence(List(Future(factorial(5)), Future(factorial(5))))

  val results: List[Int] = Await.result(futureFactorials, Duration.Inf)

  println(results)
}

object SlowFactorialWithWriterMonad {
  type Logged[A] = Writer[Vector[String], A]
  import cats.syntax.applicative._
  import cats.syntax.writer._
  import cats.instances.vector._

  def slowly[T](f: => T) = {
    try f finally Thread.sleep(100)
  }

  def factorial(n: Int): Logged[Int] = {
    def factorialWithLogged(loggedFactorial: Logged[Int]): Logged[Int] = {
      for {
        n <- loggedFactorial
        result <- slowly(if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n))
        _ <- Vector(s"n is: $n result is: $result").tell
      } yield result
    }

    factorialWithLogged(n.pure[Logged])
  }

}


object WriterMonadDemoWithWriterMonadApp extends App {
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global

  import SlowFactorialWithWriterMonad._

  var futureFactorials: Future[List[Logged[Int]]] = Future.sequence(List(Future(factorial(5)), Future(factorial(5))))

  val results: List[Logged[Int]] = Await.result(futureFactorials, Duration.Inf)

  results.map(_.run).foreach{ case (messages, result) => {
    println(messages.mkString("\n"))
    println(s"result: $result")
  }}

}
