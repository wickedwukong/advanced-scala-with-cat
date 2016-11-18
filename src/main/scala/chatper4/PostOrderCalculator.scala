package chatper4

import cats.data._

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

    def myEvalOne(sym: String): CalcState[Int] = {
      State[List[Int], Int] { stack => {
        if ((sym == "*") || (sym == "+") || (sym == "-") || (sym == "/")) {
          stack match {
            case v1 :: v2 :: tail => {
              val result = Operator(sym).apply(v1, v2)
              (result :: tail, result)
            }
          }
        } else {
          (sym.toInt :: stack, sym.toInt)
        }
      }
      }
    }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operand(num: Int): CalcState[Int] = State[List[Int], Int] { stack =>
    (num :: stack, num)
  }

  def operator(func: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case a :: b :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ => sys.error("Fail!")
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(State.pure[List[Int], Int](0)) {
      (accumulatedState, sym) => {
        accumulatedState.flatMap(_ => myEvalOne(sym))
      }
    }
  }
}

object PostOrderCalculatorApp extends App {

  import PostOrderCalculator._

  println(evalOne("42").run(Nil).value)

  val program = for {
    _ <- myEvalOne("1")
    _ <- myEvalOne("2")
    ans <- myEvalOne("+")
  } yield ans

  println(program.run(List.empty).value)

  println(evalAll(List("1", "2", "+", "3", "*")).run(Nil).value)

}
