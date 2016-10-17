package temperature.conversion.acceptance

import java.io.StringWriter

import org.specs2.mutable.{Specification, _}
import temperature.conversion.FahrenheitToCelsius

import scala.io.Source

class FahrenheitToCelsiusSpec extends Specification {


  "The application" should {
      "Should first ask to input a temperature in Fahrenheit" in {
        val output: StringWriter = new StringWriter()
        val emptyInput: Source = Source.fromIterable(List.empty)

        new FahrenheitToCelsius(emptyInput, output).run

        output.toString must_==("What is the temperature in Fahrenheit?\n")

      }
      "start with 'Hello'" in {
        "Hello world" must startWith("Hello")
      }
      "end with 'world'" in {
        "Hello world" must endWith("world")
      }
    }
}
