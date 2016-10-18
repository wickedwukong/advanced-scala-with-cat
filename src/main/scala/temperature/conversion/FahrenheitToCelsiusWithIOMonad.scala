package temperature.conversion

import java.io._

import scala.util.Try

trait IO[A] {
  self =>

  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    def run = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }
}

object IO {
  def apply[A](a: => A) = new IO[A] {
    override def run: A = a
  }
}

class InputIO(input: BufferedReader) {
  def readLine = IO {
    input.readLine()
  }
}

class OutputIO(output: Writer) {
  private val printWriter: PrintWriter = new PrintWriter(output)

  def printLine(value: String): IO[Unit] = IO{
    printWriter.println(value)
    printWriter.flush()
  }
}

object FahrenheitToCelsiusWithIOMonad {
  def fahrenheitToCelsius(temperatureInFahrenheit: Double) = (temperatureInFahrenheit - 32) * 5.0 / 9.0
}

class FahrenheitToCelsiusWithIOMonad(input: BufferedReader, output: Writer) {

  import FahrenheitToCelsiusWithIOMonad._

  val outputIO = new OutputIO(output)
  val inputIO = new InputIO(input)

  def execute= {
    for {
      _ <- outputIO.printLine("Enter a temperature in degrees Fahrenheit:")
      doubleValue <- inputIO.readLine.map(_.toDouble)
      _ <- outputIO.printLine(fahrenheitToCelsius(doubleValue).toString)
    } yield ()
  }
}
