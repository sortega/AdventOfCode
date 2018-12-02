package advent.y2018

import scala.annotation.tailrec

import advent.shared.Time.timed

object Day2 {

  def part1(input: List[String]): Int = {
    val freqs      = input.map(charFreqs)
    val numDoubles = freqs.count(f => f.values.toList.contains(2))
    val numTriples = freqs.count(f => f.values.toList.contains(3))
    numDoubles * numTriples
  }

  def charFreqs(s: String): Map[Char, Int] = s.groupBy(identity).mapValues(_.length)

  def part2(input: List[String]): String = {
    @tailrec
    def go(input: List[String], seen: Set[String]): String = input match {
      case Nil => throw new IllegalStateException("No match found")
      case s :: remaining =>
        val variants = withOneCharRemoved(s)
        val matches = variants intersect seen
        if (matches.nonEmpty) matches.head
        else go(remaining, seen union variants)
    }

    go(input, seen = Set.empty)
  }

  def withOneCharRemoved(s: String): Set[String] = s.indices.map { i =>
    val (before, after) = s.splitAt(i)
    before + after.tail
  }.toSet

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 2).getLines().toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
