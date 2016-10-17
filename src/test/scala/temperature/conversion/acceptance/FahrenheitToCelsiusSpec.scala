package temperature.conversion.acceptance

import java.io.StringWriter

import org.specs2.mutable.{Specification, _}
import temperature.conversion.FahrenheitToCelsius

import scala.io.Source

class FahrenheitToCelsiusSpec extends Specification {


  "The application" should {
      "first ask to input a temperature in Fahrenheit" in {
        val output: StringWriter = new StringWriter()
        val emptyInput: Source = Source.fromIterable(List.empty)

        new FahrenheitToCelsius(emptyInput, output).run

        output.toString must_==("What is the temperature in Fahrenheit?\n")

      }

      "32 in Fahrenheit is 0 in Celsius " in {
        val output: StringWriter = new StringWriter()
        val fahrenheit32: Source = Source.fromIterable(List(32))


        new FahrenheitToCelsius(fahrenheit32, output).run

        val expectedOutput = """What is the temperature in Fahrenheit?
                             |0""".stripMargin

        output.toString must_==(expectedOutput)
      }

      "end with 'world'" in {
        "Hello world" must endWith("world")
      }
    }
}
