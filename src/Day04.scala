object Day04 {
  lazy val ps: LazyList[Int] = 2 #:: LazyList.from(3).filter(i =>
    ps.takeWhile { j => j * j <= i }.forall { k => i % k > 0 })

  val primes: List[BigInt] = ps.take(25).toList.map(i =>BigInt.int2bigInt(i))

  val primeMatrix: List[List[BigInt]] = primes.grouped(5).toList
  val primeProducts: Set[BigInt] = primeMatrix.map(_.product).toSet ++ primeMatrix.transpose.map(_.product).toSet

  def a(input: Seq[String]): Int = {
    val (numbers, matrices) = parseInput(input)
    val elemToPrimes = matrices.map(m => (m zip primes).toMap)
    val (e2p, index, markedProduct) = elemToPrimes.map { e2p =>
      val (index, markedProduct) = indexOfWinningNum(e2p, numbers)
      (e2p, index, markedProduct)
    }.minBy(_._2)
    calculateScore(e2p, numbers(index), markedProduct)
  }

  def b(input: Seq[String]): Int = {
    val (numbers, matrices) = parseInput(input)
    val elemToPrimes = matrices.map(m => (m zip primes).toMap)
    val temp = elemToPrimes.map { e2p =>
      val (index, markedProduct) = indexOfWinningNum(e2p, numbers)
      (e2p, index, markedProduct)
    }.filter(_._2 < numbers.size)
    val (e2p, index, markedProduct) = temp.maxBy(_._2)
    calculateScore(e2p, numbers(index), markedProduct)
  }

  private def indexOfWinningNum(elemToPrime: Map[Int, BigInt], numbers: Seq[Int]): (Int, BigInt) = {
    var markedPrimeProduct: BigInt = 1

    numbers.zipWithIndex.foreach { case (num, index) =>
      if (elemToPrime.contains(num)) {
        markedPrimeProduct *= elemToPrime(num)
        if (primeProducts.exists(markedPrimeProduct % _ == 0)) {
          return (index, markedPrimeProduct)
        }
      }
    }
    (numbers.size, markedPrimeProduct)
  }


  private def calculateScore(e2p: Map[Int, BigInt], num: Int, markedPrimeProduct: BigInt): Int = {
    val unmarkedSum = e2p.filter { case (k, v) => markedPrimeProduct % v != 0 }.keys.sum
    unmarkedSum * num
  }

  //return the numbers and flattened matrices
  private def parseInput(input: Seq[String]): (Vector[Int], Vector[Vector[Int]]) = {
    val number = input.head.split(',').map(_.toInt).toVector
    val matrices = input.tail.grouped(6).map { lines =>
      //drop leading blank line
      lines.tail.map(_.trim.split("\\s+").toList).flatMap { numStr =>
        numStr.map(_.toInt)
      }.toVector
    }.toVector

    (number, matrices)
  }
}
