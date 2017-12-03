package advent.y2017

import advent.shared.Time.timed

object Day2 {

  def part1(input: List[List[Int]]): Int = sumBy(input, rangeOf)

  def part2(input: List[List[Int]]): Int = sumBy(input, wholeDivisionIn)

  private def rangeOf(row: List[Int]): Int = row.max - row.min

  private def sumBy[A](input: List[A], f: A => Int) = input.map(f).sum

  private def wholeDivisionIn(row: List[Int]): Int =
    row.toStream
      .combinations(2)
      .map(_.toList.sorted)
      .collectFirst {
        case List(a, b) if b % a == 0 => b / a
      }
      .get

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 2).getLines().map(_.split("\\s+").map(_.toInt).toList).toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
