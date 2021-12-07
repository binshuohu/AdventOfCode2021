object Day07 {
  def a(input: Seq[String]): Int = {
    val positions = parseInput(input)
    calc(positions, identity)
  }

  def b(input: Seq[String]): Int = {
    val positions = parseInput(input)
    calc(positions, i => i * (i + 1) / 2)
  }

  private def calc(positions: Seq[Int], costFunc: Int => Int): Int = {
    (positions.min to positions.max).map(target => positions.map(p => costFunc.apply(Math.abs(p - target))).sum).min
  }

  private def parseInput(input: Seq[String]): Seq[Int] = {
    input.head.split(',').map(_.toInt)
  }
}
