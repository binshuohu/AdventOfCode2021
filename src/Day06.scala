object Day06 {
  def a(input: Seq[String]): BigInt = {
    val initial = parseInput(input)
    initial.map(spawn(_, 80)).sum
  }

  def b(input: Seq[String]): BigInt = {
    val initial = parseInput(input)
    initial.map(spawn(_, 256)).sum
  }

  private val memo = collection.mutable.HashMap.empty[(Int, Int), BigInt]

  private def spawn(timer: Int, days: Int): BigInt = {
    if (timer >= days) {
      1
    } else if (memo.contains(timer, days)) {
      memo((timer, days))
    } else {
      val result = spawn(6, days - timer - 1) + spawn(8, days - timer - 1)
      memo((timer, days)) = result
      result
    }
  }

  private def parseInput(input: Seq[String]): Seq[Int] = {
    input.head.split(',').map(_.toInt)
  }
}
