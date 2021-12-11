object Day10 {
  def a(input: Seq[String]): Int = {
    val scoreMap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    input.map(processLine).collect { case Corrupted(c) => scoreMap(c) }.sum
  }

  def b(input: Seq[String]): BigInt = {
    val scoreMap = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
    val scores = input.toArray.map(processLine).collect { case Incomplete(stack) =>
      stack.toSeq.map(scoreMap).foldLeft(BigInt.int2bigInt(0))((z, elem) => z * 5 + elem)
    }.sorted
    scores(scores.length / 2)
  }

  private val mapping = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  private def processLine(line: String): Result = {
    val stack = collection.mutable.Stack.empty[Char]
    line.foreach {
      case char@('(' | '[' | '{' | '<') =>
        stack.push(char)
      case char =>
        if (stack.isEmpty || stack.pop() != mapping(char)) {
          return Corrupted(char)
        }
    }
    if (stack.isEmpty) OK else Incomplete(stack)
  }

  trait Result

  case object OK extends Result

  case class Incomplete(stack: collection.mutable.Stack[Char]) extends Result

  case class Corrupted(char: Char) extends Result

}
