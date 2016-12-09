package advent.y2015

object Day1 {

  val increments = Map('(' -> 1, ')' -> -1)

  def part1(input: String): Int = input.map(increments).sum

  def part2(input: String): Int = input.map(increments).scanLeft(0)(_ + _).indexOf(-1)

  def main(args: Array[String]): Unit = {
    val basicInput = ""
    val difficultInput = ""
    println("Part 1 result: " + part1(basicInput))
    println("Part 2 result: " + part2(difficultInput))
  }
}
