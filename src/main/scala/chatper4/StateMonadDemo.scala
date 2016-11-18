package chatper4

object StateMonadDemo extends App {

  import cats.data.State

  val step1 = State[Int, String] { num => val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num => val ans = num * 2
    (ans, s"Result of step2: $ans")
  }


  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state, result) = both.run(20).value

  println(state)
  println(result)


  val bothWithoutForSyntaxSugar = step1.flatMap(a => {
    step2.map { b =>
      (a, b)
    }
  })

  val value: (Int, (String, String)) = bothWithoutForSyntaxSugar.run(20).value
  println(value)


}
