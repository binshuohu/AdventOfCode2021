object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = "/Users/binshuo/input.txt"

    val input = TextReader.readFromFile(inputFile)
    println(Day04.b(input))
  }
}
