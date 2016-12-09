package advent.y2015

object Day10 {

  def pronounce(number: String): String = pronounce(number.toStream).mkString

  def pronounce(number: Stream[Char]): Stream[Char] =
    if (number.isEmpty) number
    else {
      val (prefix, suffix) = number.span(_ == number.head)
      prefix.length.toString.toStream ++ number.head #:: pronounce(suffix)
    }

  def part1(input: String): Int = lengthAfterRepeatedPronunciation(input, 40)

  def part2(input: String): Int = lengthAfterRepeatedPronunciation(input, 50)

  private def lengthAfterRepeatedPronunciation(input: String, times: Int): Int =
    Stream.iterate(input)(pronounce).apply(times).length

  def main(args: Array[String]): Unit = {
    val input = "1321131112"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
