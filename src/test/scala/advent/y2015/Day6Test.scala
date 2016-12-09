package advent.y2015

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day6Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "track commands changing lights" in {
    Day6.part1(
      "turn on 0,0 through 1,1\n" +
      "turn off 0,0 through 0,0\n" +
      "turn off 0,0 through 0,0\n" +
      "toggle 1,0 through 1,1\n"
    ) shouldBe 1
  }

  "Part 2" should "track commands changing light intensity" in {
    Day6.part2(
      "turn on 0,0 through 1,1\n" +
      "turn off 0,0 through 0,0\n" +
      "turn off 0,0 through 0,0\n" +
      "toggle 1,0 through 1,1\n"
    ) shouldBe 7

    Day6.part2(
      "turn on 0,0 through 999,999\n" +
      "toggle 0,0 through 999,999\n"
    ) shouldBe 3000000
  }
}
