package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day2Test extends FlatSpec with ShouldMatchers {

  "Part 1 wrapping paper" should "compute the wrapping paper" in {
    Day2.part1("2x3x4") shouldBe 58
    Day2.part1("1x1x10") shouldBe 43
  }

  it should "add up the area for all presents" in {
    Day2.part1("2x3x4\n1x1x10\n") shouldBe 58 + 43
  }

  "Part 2 wrapping paper" should "compute the ribbon length" in {
    Day2.part2("2x3x4") shouldBe 34
    Day2.part2("1x1x10") shouldBe 14
  }

  it should "add up the length for all presents" in {
    Day2.part2("2x3x4\n1x1x10\n") shouldBe 34 + 14
  }
}
