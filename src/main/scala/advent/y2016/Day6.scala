package advent.y2016

object Day6 {

  def part1(input: String): String =
    columnsOf(input).map(mostFrequent).mkString

  private def mostFrequent(letters: List[Char]): Char =
    frequencies(letters).toList.sortBy(-_._2).head._1

  private def leastFrequent(letters: List[Char]): Char =
    frequencies(letters).toList.sortBy(_._2).head._1

  private def frequencies(letters: List[Char]) =
    letters.groupBy(identity).mapValues(_.size)

  private def columnsOf(input: String) =
    input.lines.map(_.toList).toList.transpose

  def part2(input: String): String = columnsOf(input).map(leastFrequent).mkString

  def main(args: Array[String]): Unit = {
    val input = dailyResource(6).mkString
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
