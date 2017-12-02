package advent.y2016

import advent.y2016.Day20.Range
import org.scalatest.{FlatSpec, Matchers}

class Day20Test extends FlatSpec with Matchers {

  "Part 1" should "find the lowest-valued IP that is not blocked" in {
    val blacklist = List(
      Range(5, 8),
      Range(0, 2),
      Range(4, 7)
    )
    Day20.part1(blacklist) shouldBe 3
  }

  "Part 2" should "???" in {}
}
