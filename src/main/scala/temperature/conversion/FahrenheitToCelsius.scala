package temperature.conversion

import java.io._

import scala.io.{Source, StdIn}
import scala.util.Try
class FahrenheitToCelsius(input: BufferedReader, output: Writer) {

  def toDouble(value: String): Option[Double] = Try {value.toDouble}.toOption

  def run: Unit = {
    val outputPrintWriter = new PrintWriter(output)

    outputPrintWriter.println("Enter a temperature in degrees Fahrenheit:")
    outputPrintWriter.flush()

    toDouble(input.readLine()).map{
      temperatureInFahrenheit => (temperatureInFahrenheit - 32) * 5.0/9.0
    }.foreach {tempatrueInCelsius =>
      outputPrintWriter.println(tempatrueInCelsius)
      outputPrintWriter.flush()
    }
  }
}
