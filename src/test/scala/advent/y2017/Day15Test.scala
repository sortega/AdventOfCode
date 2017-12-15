package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {

  "Part 1" should "count matching pairs" in {
    Day15.part1(65, 8921) shouldBe 588
  }

  "Part 2" should "count matching pairs generated with more restrictions" in {
    Day15.part2(65, 8921) shouldBe 309
  }
}
