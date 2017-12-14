package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {

  val input = "nbysizxe"

  "Part 1" should "count the number of occupied squares" in {
    Day14.part1(input) shouldBe 8216
  }

  "Part 2" should "???" in {
    Day14.part2(input) shouldBe 1139
  }
}
