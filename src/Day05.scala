object Day05 {

  def a(input: Seq[String]): Int = {
    val lines = parseInput(input).filter(line => line.isHorizontal || line.isVertical)
    count(lines)
  }

  def b(input: Seq[String]): Int = {
    val lines = parseInput(input)
    count(lines)
  }

  private def count(lines: Seq[Line]): Int = {
    val grid = scala.collection.mutable.HashMap.empty[Point, Int]
    (for {
      line <- lines
      point <- line.points
    } yield point) foreach { point =>
      grid(point) = grid.getOrElse(point, 0) + 1
    }
    grid.values.count(_ > 1)
  }

  private def parseInput(input: Seq[String]): Seq[Line] = {
    val pattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
    input map {
      case pattern(x1, y1, x2, y2) => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }
  }

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {
    def isHorizontal: Boolean = {
      start.y == end.y
    }

    def isVertical: Boolean = {
      start.x == end.x
    }

    def points: Iterable[Point] = {
      if (isHorizontal) {
        for (x <- start.x to end.x by (end.x - start.x) / Math.abs(end.x - start.x))
          yield Point(x, start.y)
      } else if (isVertical) {
        for (y <- start.y to end.y by (end.y - start.y) / Math.abs(end.y - start.y))
          yield Point(start.x, y)
      } else {
        val xStep = (end.x - start.x) / Math.abs(end.x - start.x)
        val yStep = (end.y - start.y) / Math.abs(end.y - start.y)
        for ((x, y) <- (start.x to end.x by xStep) zip (start.y to end.y by yStep))
          yield Point(x, y)
      }
    }
  }
}
