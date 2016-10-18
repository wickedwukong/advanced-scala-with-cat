package temperature.conversion.acceptance

import java.io.{BufferedReader, StringReader, StringWriter}

import org.specs2.mutable.{Specification, _}
import temperature.conversion.FahrenheitToCelsius

import scala.io.Source

class FahrenheitToCelsiusSpec extends Specification {


  "The application" should {
    "prompt for a temperature in Fahrenheit" in {
      val output: StringWriter = new StringWriter()
      val emptyInput: BufferedReader = new BufferedReader(new StringReader(""))


      new FahrenheitToCelsius(emptyInput, output).run

      output.toString must_== ("Enter a temperature in degrees Fahrenheit:\n")
    }

    "32 in Fahrenheit is 0 in Celsius " in {
      val output: StringWriter = new StringWriter()
      val fahrenheit32: BufferedReader = new BufferedReader(new StringReader("32"))

      new FahrenheitToCelsius(fahrenheit32, output).run

      val expectedOutput =
        """Enter a temperature in degrees Fahrenheit:
          |0.0
          |""".stripMargin

      output.toString must_== (expectedOutput)
    }

    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
  }
}
