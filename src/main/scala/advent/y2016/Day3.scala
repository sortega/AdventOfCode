package advent.y2016

object Day3 {

  private def parse(line: String): Seq[Int] = line.split("\\s+").filterNot(_.isEmpty).map(_.toInt)

  private def triangular(sides: Seq[Int]): Boolean = {
    val Seq(a, b, c) = sides.sorted
    a + b > c
  }

  def part1(input: Seq[String]): Int = input.map(parse).count(triangular)

  def part2(input: Seq[String]): Int = {
    val candidates = input.map(parse).transpose.flatten.grouped(3)
    candidates.count(triangular)
  }

  def main(args: Array[String]): Unit = {
    val input = dailyResource(3).getLines().toSeq
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
