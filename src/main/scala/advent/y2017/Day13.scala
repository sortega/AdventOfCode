package advent.y2017

import advent.shared.Time.timed

object Day13 {

  case class Scanner(depth: Int, range: Int) {
    val period: Int   = (range - 1) * 2
    val severity: Int = depth * range

    def posAt(t: Int): Int = {
      val relT = t % period
      if (relT < range) relT
      else period - relT
    }

    def caughtAt(t: Int): Boolean = posAt(t) == 0
  }

  case class Firewall(scanners: List[Scanner]) {
    def severityOfCrossingAt(t: Int): Int = {
      val severities = scanners.map { scanner =>
        if (scanner.caughtAt(t + scanner.depth))
          scanner.severity
        else 0
      }
      severities.sum
    }

    def minDelayToCrossWithoutBeingCaught: Int =
      scanners.foldLeft(Stream.from(0)) { (delays, scanner) =>
        delays.filterNot(delay => scanner.caughtAt(delay + scanner.depth))
      }.head
  }

  object Firewall {
    def parse(input: String): Firewall =
      Firewall(
        input.lines
          .filter(_.trim.nonEmpty)
          .map { line =>
            val Array(depth, range) = line.split(":").map(_.trim.toInt)
            Scanner(depth, range)
          }
          .toList)
  }

  def part1(input: String): Int = Firewall.parse(input).severityOfCrossingAt(0)

  def part2(input: String): Int = Firewall.parse(input).minDelayToCrossWithoutBeingCaught

  def main(args: Array[String]): Unit = {
    val input =
      """0: 3
        |1: 2
        |2: 4
        |4: 6
        |6: 5
        |8: 6
        |10: 6
        |12: 4
        |14: 8
        |16: 8
        |18: 9
        |20: 8
        |22: 6
        |24: 14
        |26: 12
        |28: 10
        |30: 12
        |32: 8
        |34: 10
        |36: 8
        |38: 8
        |40: 12
        |42: 12
        |44: 12
        |46: 12
        |48: 14
        |52: 14
        |54: 12
        |56: 12
        |58: 12
        |60: 12
        |62: 14
        |64: 14
        |66: 14
        |68: 14
        |70: 14
        |72: 14
        |80: 18
        |82: 14
        |84: 20
        |86: 14
        |90: 17
        |96: 20
        |98: 24
        |""".stripMargin
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
