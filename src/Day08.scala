object Day08 {
  def a(input: Seq[String]): Int = {
    val q1UniqueNumSegs = Set(2, 3, 4, 7)
    parseInput(input).flatMap(_._2).count(str => q1UniqueNumSegs.contains(str.length))
  }

  def b(input: Seq[String]): Int = {
    parseInput(input).map { entry => bProcessEntry(entry._1.map(_.sorted), entry._2.map(_.sorted)) }.sum
  }

  private def bProcessEntry(input: Seq[String], output: Seq[String]): Int = {
    val s1 = input.find(_.length == 2).get
    val s4 = input.find(_.length == 4).get
    val s7 = input.find(_.length == 3).get
    val s8 = input.find(_.length == 7).get
    val s9 = input.find(str => str.length == 6 && s4.toSet.subsetOf(str.toSet)).get
    val s0 = input.find(str => str.length == 6 && str != s9 && s1.toSet.subsetOf(str.toSet)).get
    val s6 = input.find(str => str.length == 6 && str != s9 && str != s0).get
    val s3 = input.find(str => str.length == 5 && s1.toSet.subsetOf(str.toSet)).get
    val s5 = input.find(str => str.length == 5 && str.toSet.subsetOf(s6.toSet)).get
    val s2 = input.find(str => str.length == 5 && !Set(s3, s5).contains(str)).get

    val mapping = Map(s0 -> 0, s1 -> 1, s2 -> 2, s3 -> 3, s4 -> 4,
      s5 -> 5, s6 -> 6, s7 -> 7, s8 -> 8, s9 -> 9)

    output.map(mapping.apply).map(_.toString).mkString.toInt
  }

  private def parseInput(input: Seq[String]): Seq[(Seq[String], Seq[String])] = {
    input.map { line =>
      val parts = line.split('|')
      (parts(0).trim.split("\\s").toSeq, parts(1).trim.split("\\s").toSeq)
    }
  }
}
