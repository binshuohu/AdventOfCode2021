object Day01 {
  def a(input: Seq[String]): Int = {
    val readings = input.map(_.toInt)
    countIncrease(readings)
  }

  def b(input: Seq[String]): Int = {
    val readings = input.map(_.toInt)
    val slidingWindowSum = readings.sliding(3).map(_.sum).toArray
    countIncrease(slidingWindowSum)
  }

  private def countIncrease(input: Seq[Int]): Int = {
    (input.tail zip input) count (_ - _ > 0)
  }
}
