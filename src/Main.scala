import Day05.Line

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile = "/Users/binshuo/input.txt"

    val input = TextReader.readFromFile(inputFile)
    println(Day06.a(input))
    println(Day06.b(input))
  }
}
