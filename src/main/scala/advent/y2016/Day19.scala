package advent.y2016

import scala.annotation.tailrec

object Day19 {

  def part1(n: Int): Int = n match {
    case 1 | 2 => 1
    case even if even % 2 == 0 => 2 * part1(even / 2) - 1
    case odd => 2 * part1(odd / 2) + 1
  }

  def part2(input: Int): Int = {
    @tailrec
    def steal(elves: Vector[Int]): Int =
      if (elves.size <= 2) {
        elves.head
      } else if (elves.size >= 3 && elves.size % 3 == 0) {
        steal(elves.grouped(3).map(_.last).toVector)
      } else {
        val stealFrom = elves.size / 2
        val remainingElves =
          elves.slice(1, stealFrom) ++ elves.slice(stealFrom + 1, elves.size) :+ elves.head
        steal(remainingElves)
      }

    steal(elves = Vector.range(1, input + 1))
  }

  def main(args: Array[String]): Unit = {
    val input = 3014387
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
