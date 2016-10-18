package temperature.conversion

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}

object FahrenheitToCelsiusApp extends App {
  val input = new BufferedReader(new InputStreamReader(java.lang.System.in))

  new FahrenheitToCelsius(input, new OutputStreamWriter(System.out)).run
}
