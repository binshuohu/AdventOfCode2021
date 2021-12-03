import scala.annotation.tailrec

object Day03 {
  def a(input: Seq[String]): Int = {
    val binaries = parseInput(input)
    val counter = count1s(binaries)
    val gamma = Integer.parseInt(counter.map { e =>
      if (e * 2 > input.size) 1 else 0
    }.mkString, 2)

    //epsilon equals to gamma with all bits flipped
    val mask = (1 << 12) - 1
    val epsilon = (gamma ^ (-1)) & mask
    gamma * epsilon
  }

  def b(input: Seq[String]): Int = {
    val binaries = parseInput(input)
    val counter = count1s(binaries)

    // bitSelector is (count, seqSize) => bit(0 or 1)
    @tailrec
    def recurse(counter: Seq[Int], arr: Seq[Seq[Int]], pos: Int, predicateToSelect1: (Int, Int) => Boolean): Seq[Int] = arr match {
      case Seq(mono) => mono
      case _ =>
        val bit = if (predicateToSelect1(counter(pos), arr.size)) 1 else 0
        val (newCounter, newArr) = arr.foldLeft((counter, Seq.empty[Seq[Int]])) {
          case ((runningCounter, runningArr), elem) =>
            if (elem(pos) == bit) {
              (runningCounter, elem +: runningArr)
            } else {
              ((runningCounter zip elem).map(_ - _), runningArr)
            }
        }
        recurse(newCounter, newArr, pos + 1, predicateToSelect1)
    }

    val oxygenRating = Integer.parseInt(recurse(counter, binaries, 0, _ * 2 >= _).mkString, 2)
    val co2Rating = Integer.parseInt(recurse(counter, binaries, 0, _ * 2 < _).mkString, 2)
    oxygenRating * co2Rating
  }

  private def parseInput(input: Seq[String]): Seq[Seq[Int]] = {
    input.map(s => s.map(_.toString.toInt))
  }

  private def count1s(binaries: Seq[Seq[Int]]) = {
    binaries.transpose.map(_.sum)
  }
}
