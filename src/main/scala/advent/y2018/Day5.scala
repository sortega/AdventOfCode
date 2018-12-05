package advent.y2018

import scala.annotation.tailrec

import advent.shared.Time.timed

object Day5 {

  def part1(polymer: String): Int = reduce(polymer).length

  def reduce(polymer: String): String = {
    @tailrec
    def go(prefix: List[Char], suffix: List[Char]): String =
      (prefix, suffix) match {
        case (_, Nil)                             => prefix.reverse.mkString
        case (p :: ps, s :: ss) if canReact(p, s) => go(ps, ss)
        case (_, s :: ss)                         => go(s :: prefix, ss)
      }

    go(prefix = Nil, suffix = polymer.toList)
  }

  def canReact(left: Char, right: Char): Boolean =
    (left != right) && (left.toUpper == right.toUpper)

  def part2(polymer: String): Int = {
    val normallyReduced = reduce(polymer)
    val elements = normallyReduced.toLowerCase.toSet
    elements.map { element =>
      part1(normallyReduced.filter(c => c.toLower != element))
    }.min
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 5).getLines().mkString.trim
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
