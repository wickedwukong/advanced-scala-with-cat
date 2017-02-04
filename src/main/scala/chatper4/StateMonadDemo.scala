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

object Experiment extends App {

  trait Check[A] {
    def check(a: A): Int
  }

  trait CheckBinding[A] {
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

  class MaxRowCheckBinding(ints: => Seq[Int]) extends CheckBinding[Int] {
    override def a: Int = AProvider.intA(ints)

    override def getCheck: Check[Int] = new MaxRowCheck()
  }

  class MinRowCheckBinding(ints: => Seq[Int]) extends CheckBinding[String] {
    override def a: String = AProvider.stringA(ints)

    override def getCheck: Check[String] = new MinRowCheck()
  }

  def getInts: Seq[Int] = Seq(1, 2, 3)

  val checkBindMap: Map[String, CheckBinding[_]] = Map(
    "MaxRowCheck" -> new MaxRowCheckBinding(getInts),
    "MinRowCheck" -> new MinRowCheckBinding(getInts))

  private val map: Option[Int] = checkBindMap.get("MinRowCheck").map { bind => {
    bind.doIt
  }
  }
  println(map)
}


object Experiment2 extends App {

  trait Check[A] {
    def check(a: A): Int
  }

  class MaxRowCheck extends Check[Int] {
    override def check(a: Int): Int = 1
  }

  class MinRowCheck extends Check[String] {
    override def check(a: String): Int = 2
  }

  trait CheckBinding {
    type A
    def getA: A
    def getCheck: Check[A]
  }

  object AProvider {
    def intA(ints: Seq[Int]): Int = 1

    def stringA(ints: Seq[Int]): String = "hello"
  }

  def getInts: Seq[Int] = Seq(1, 2, 3)

  class MinRowCheckBinding(ints: => Seq[Int]) extends CheckBinding {
    override type A = String

    override def getCheck: Check[String] = new MinRowCheck()

    override def getA: String = AProvider.stringA(ints)
  }

  class MaxRowCheckBinding(ints: => Seq[Int]) extends CheckBinding {
    override type A = Int

    override def getA: Int = AProvider.intA(ints)

    override def getCheck: Check[Int] = new MaxRowCheck()
  }

  val checkBindMap: Map[String, CheckBinding] = Map(
    "MaxRowCheck" -> new MaxRowCheckBinding(getInts),
    "MinRowCheck" -> new MinRowCheckBinding(getInts))

  private val map: Option[Int] = checkBindMap.get("MaxRowCheck").map { bind => {
    bind.getCheck.check(bind.getA)
  }}

  println(map)

}