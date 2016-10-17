package temperature.conversion

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}

object FahrenheitToCelsiusWithIOMonadApp extends App {
  val input = new BufferedReader(new InputStreamReader(java.lang.System.in))
  new FahrenheitToCelsiusWithIOMonad(input, new OutputStreamWriter(System.out)).run
}
