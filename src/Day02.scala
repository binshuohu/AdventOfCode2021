object Day02 {
  def a(input: Seq[String]): Int = {
    val (posX, posY) = parseInput(input).foldLeft((0, 0)) {
      case ((x, y), (dx, dy)) => (x + dx, (y + dy) max 0)
    }
    posX * posY
  }

  def b(input: Seq[String]): Int = {
    val ((posX, posY), _) = parseInput(input).foldLeft(((0, 0), 0)) {
      case ((pos, aim), (0, delta)) => (pos, (aim + delta) max 0)
      case (((x, y), aim), (delta, 0)) => ((x + delta, y + delta * aim), aim)
    }
    posX * posY
  }

  private def parseInput(input: Seq[String]): Seq[(Int, Int)] = {
    val forward = "forward (\\d+)".r
    val up = "up (\\d+)".r
    val down = "down (\\d+)".r
    input map {
      case forward(v) => (v.toInt, 0)
      case up(v) => (0, -v.toInt)
      case down(v) => (0, v.toInt)
    }
  }
}
