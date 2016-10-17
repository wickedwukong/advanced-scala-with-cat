package temperature.conversion

import java.io.{PrintWriter, StringWriter, Writer}

import scala.io.Source

class FahrenheitToCelsius(input: Source, output: Writer) {

  def run: Unit = {
    new PrintWriter(output).println("What is the temperature in Fahrenheit?")
  }

}
