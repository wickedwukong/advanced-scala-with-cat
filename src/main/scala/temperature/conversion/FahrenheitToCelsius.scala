package temperature.conversion

import java.io._

import scala.io.{Source, StdIn}
import scala.util.Try

object TemperatureConverter {
  def fahrenheitToCelsius(temperatureInFahrenheit: Double) =
    (temperatureInFahrenheit - 32) * 5.0 / 9.0
}

class FahrenheitToCelsius(input: BufferedReader, output: Writer) {

  import TemperatureConverter._

  private val outputPrintWriter = new PrintWriter(output)

  def toDouble(value: String): Option[Double] = Try {
    value.toDouble
  }.toOption

  private def printValue(value: Any) = {
    outputPrintWriter.println(value)
    outputPrintWriter.flush()
  }

  private def printPrompt = printValue("Enter a temperature in degrees Fahrenheit:")

  def run: Unit = {
    printPrompt

    toDouble(input.readLine()).map(fahrenheitToCelsius(_)).foreach {
      printValue(_)
    }
  }
}

