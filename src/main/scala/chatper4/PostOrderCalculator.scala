package chatper4

object Operator {
  def apply(sym: String): Operator = {
    if (sym == "*") Multiply
    else if (sym == "+") Plus
    else if (sym == "/") Divide
    else Minus
  }
}

sealed trait Operator {
  def apply(v1: Int, v2: Int): Int
}

object Plus extends Operator {
  override def apply(v1: Int, v2: Int): Int = v1 + v2
}

object Minus extends Operator {
  override def apply(v1: Int, v2: Int): Int = v1 - v2
}

object Divide extends Operator {
  override def apply(v1: Int, v2: Int): Int = v1 / v2
}

object Multiply extends Operator {
  override def apply(v1: Int, v2: Int): Int = v1 * v2
}


object PostOrderCalculator {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] { stack => {
      if ((sym == "*") || (sym == "+") || (sym == "-") || (sym == "/")) {
        stack match {
          case v1 :: v2 :: tail => {
            val result = Operator(sym).apply(v1, v2)
            (tail, result)
          }
        }
      } else {
        (sym.toInt :: stack, sym.toInt)
      }
    }
    }
  }
 }

object PostOrderCalculatorApp extends App {
  import PostOrderCalculator._
  println(evalOne("42").run(Nil).value)

  val program = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(program.run(List.empty).value)

}
