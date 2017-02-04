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

  trait ValueCheck[A] {
    def check(a: A): Int
  }

  trait CheckBinding[A] {
    def getValue: A

    def getCheck: ValueCheck[A]

    def doCheckValue: Int = getCheck.check(getValue)
  }

  class MaxRowValueCheck extends ValueCheck[Int] {
    override def check(a: Int): Int = 1
  }

  class MinRowValueCheck extends ValueCheck[String] {
    override def check(a: String): Int = 2
  }

  object AProvider {
    def intA(ints: Seq[Int]): Int = 1

    def stringA(ints: Seq[Int]): String = "hello"
  }

  class MaxRowCheckBinding(ints: => Seq[Int]) extends CheckBinding[Int] {
    override def getValue: Int = AProvider.intA(ints)

    override def getCheck: ValueCheck[Int] = new MaxRowValueCheck()
  }

  class MinRowCheckBinding(ints: => Seq[Int]) extends CheckBinding[String] {
    override def getValue: String = AProvider.stringA(ints)

    override def getCheck: ValueCheck[String] = new MinRowValueCheck()
  }

  def getInts: Seq[Int] = Seq(1, 2, 3)

  val checkBindMap: Map[String, CheckBinding[_]] = Map(
    "MaxRowCheck" -> new MaxRowCheckBinding(getInts),
    "MinRowCheck" -> new MinRowCheckBinding(getInts))

  private val map: Option[Int] = checkBindMap.get("MinRowCheck").map { bind => {
    bind.doCheckValue
  }
  }
  println(map)
}


object Experiment2 extends App {

  trait ValueCheck[A] {
    def check(a: A): Int
  }

  class MaxRowValueCheck extends ValueCheck[Int] {
    override def check(a: Int): Int = 1
  }

  class MinRowValueCheck extends ValueCheck[String] {
    override def check(a: String): Int = 2
  }

  trait CheckBinding {
    type A
    def getValue: A
    def getValueCheck: ValueCheck[A]
  }

  object AProvider {
    def intA(ints: Seq[Int]): Int = 1

    def stringA(ints: Seq[Int]): String = "hello"
  }

  def getInts: Seq[Int] = Seq(1, 2, 3)

  class MinRowCheckBinding(ints: => Seq[Int]) extends CheckBinding {
    override type A = String

    override def getValueCheck: ValueCheck[String] = new MinRowValueCheck()

    override def getValue: String = AProvider.stringA(ints)
  }

  class MaxRowCheckBinding(ints: => Seq[Int]) extends CheckBinding {
    override type A = Int

    override def getValue: Int = AProvider.intA(ints)

    override def getValueCheck: ValueCheck[Int] = new MaxRowValueCheck()
  }

  val checkBindMap: Map[String, CheckBinding] = Map(
    "MaxRowCheck" -> new MaxRowCheckBinding(getInts),
    "MinRowCheck" -> new MinRowCheckBinding(getInts))

  private val map: Option[Int] = checkBindMap.get("MaxRowCheck").map { bind => {
    bind.getValueCheck.check(bind.getValue)
  }}

  println(map)

}