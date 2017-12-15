package advent.y2017

import advent.shared.Streams
import advent.shared.Time.timed

object Day15 {

  private val Modulus = 2147483647L
  private val FactorA = 16807L
  private val FactorB = 48271L

  def part1(seedA: Long, seedB: Long): Int =
    countMatching(generator(seedA, FactorA), generator(seedB, FactorB), n = 40000000)

  def part2(seedA: Int, seedB: Int): Int =
    countMatching(selectMultiples(generator(seedA, FactorA), 4),
                  selectMultiples(generator(seedB, FactorB), 8),
                  n = 5000000)

  private def countMatching(left: Stream[Long], right: Stream[Long], n: Int): Int =
    Streams.length(
      left
        .zip(right)
        .take(n)
        .filter((matching _).tupled))

  private def generator(seed: Long, factor: Long): Stream[Long] =
    Stream.iterate(seed)(_ * factor % Modulus).tail

  private def matching(left: Long, right: Long): Boolean =
    lowerBits(left) == lowerBits(right)

  private def lowerBits(n: Long): Long = n & 0xFFFF

  private def selectMultiples(stream: Stream[Long], n: Int): Stream[Long] =
    stream.filter(_ % n == 0)

  def main(args: Array[String]): Unit = {
    val seedA = 699
    val seedB = 124
    timed(println("Part 1 result: " + part1(seedA, seedB)))
    timed(println("Part 2 result: " + part2(seedA, seedB)))
  }
}
