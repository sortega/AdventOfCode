package advent.y2018

import advent.shared.Time.timed

object Day1 {

  def part1(shifts: List[Long]): Long = shifts.sum

  def part2(shifts: List[Long]): Long = {
    val infiniteShifts = Stream.continually(shifts).flatten
    val frequencies = infiniteShifts.scanLeft(0L)(_ + _)
    val seenFrequencies = frequencies.scanLeft(Set.empty[Long])(_ + _)
    frequencies.tail.zip(seenFrequencies).collectFirst {
      case (freq, seenFreqs) if seenFreqs.contains(freq) => freq
    }.get
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 1).getLines().map(_.toLong).toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
