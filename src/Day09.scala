object Day09 {
  def a(input: Seq[String]): Int = {
    val matrix = parseInput(input)
    val lowPointCoors = lowPointCoordinates(matrix)
    val lowPoints = lowPointCoors.map { case (i, j) => matrix(i)(j) }
    lowPoints.sum + lowPoints.size
  }

  def b(input: Seq[String]): Int = {
    val matrix = parseInput(input)
    val lowPointCoors = lowPointCoordinates(matrix)
    lowPointCoors.map { case (i, j) => basinSize(i, j, matrix) }.sorted.reverse.take(3).product
  }

  private def lowPointCoordinates(matrix: Array[Array[Int]]): Seq[(Int, Int)] = {
    val numRows = matrix.length
    val numCols = matrix(0).length
    for (i <- 0 until numRows;
         j <- 0 until numCols if isLowPoint(i, j, matrix)) yield (i, j)
  }

  def getVal(i: Int, j: Int, matrix: Array[Array[Int]]): Int = {
    val numRows = matrix.length
    val numCols = matrix(0).length
    if (i < 0 || j < 0 || i == numRows || j == numCols) Int.MaxValue
    else matrix(i)(j)
  }

  private def isLowPoint(i: Int, j: Int, matrix: Array[Array[Int]]): Boolean = {
    val d = Seq((0, 1), (0, -1), (1, 0), (-1, 0))
    d.map { case (di, dj) => getVal(i + di, j + dj, matrix) }.forall(_ > matrix(i)(j))
  }

  private def basinSize(i: Int, j: Int, matrix: Array[Array[Int]]): Int = {
    val basin = collection.mutable.Set((i, j))

    val q = collection.mutable.Queue((i, j))

    val d = Seq((0, 1), (0, -1), (1, 0), (-1, 0))

    while (q.nonEmpty) {
      val (x, y) = q.dequeue()
      for ((dx, dy) <- d) {
        val (x1, y1) = (x + dx, y + dy)

        if (!basin.contains((x1, y1))) {
          val v = getVal(x1, y1, matrix)
          if (v < 9 && v >= getVal(x, y, matrix)) {
            basin.add((x1, y1))
            q.enqueue((x1, y1))
          }
        }
      }
    }

    basin.size
  }

  private def parseInput(input: Seq[String]): Array[Array[Int]] = {
    input.map { line =>
      line.map(_.toString.toInt).toArray
    }.toArray
  }
}
