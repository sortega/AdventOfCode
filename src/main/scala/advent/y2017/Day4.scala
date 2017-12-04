package advent.y2017

import advent.shared.Time.timed

object Day4 {

  def part1(input: Seq[String]): Int =
    input.map(parseWords).count(nonRepeatedWords)

  def part2(input: Seq[String]): Int =
    input.map(parseWords _ andThen normalizeWords).count(nonRepeatedWords)

  private def parseWords(passphrase: String) = passphrase.split("\\s+").toList

  private def normalizeWords(passphrase: List[String]): List[String] = passphrase.map(_.sorted)

  private def nonRepeatedWords(passphrase: List[String]): Boolean = {
    val uniqueWords = passphrase.distinct
    passphrase.size == uniqueWords.size
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 4).getLines().toStream
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
