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

object Experiment extends App{
  trait Check[A] {
    def check(a: A): Int
  }

  trait CheckBind[A] {
    def a: A

    def getCheck: Check[A]

    def doIt: Int = getCheck.check(a)
  }

  class MaxRowCheck extends Check[Int] {
    override def check(a: Int): Int = 1
  }

  class MinRowCheck extends Check[String] {
    override def check(a: String): Int = 2
  }

  object AProvider {
    def intA(ints: Seq[Int]): Int = 1

    def stringA(ints: Seq[Int]): String = "hello"
  }

  class MaxRowCheckBind(ints: => Seq[Int]) extends CheckBind[Int] {
    override def a: Int = AProvider.intA(ints)

    override def getCheck: Check[Int] = new MaxRowCheck()
  }

  class MinRowCheckBind(ints: => Seq[Int]) extends CheckBind[String] {
    override def a: String = AProvider.stringA(ints)

    override def getCheck: Check[String] = new MinRowCheck()
  }

  def getInts: Seq[Int] = Seq(1, 2, 3)
  val checkBindMap: Map[String, CheckBind[_]] = Map(
    "MaxRowCheck" -> new MaxRowCheckBind(getInts),
    "MinRowCheck" -> new MinRowCheckBind(getInts))

  private val map: Option[Int] = checkBindMap.get("MinRowCheck").map { bind => {
    bind.doIt
  }
  }
  println(map)


}