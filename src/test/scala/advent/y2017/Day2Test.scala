package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day2Test extends FlatSpec with Matchers {

  "Part 1" should "should use row ranges as checksums" in {
    Day2.part1(List(List(5, 1, 9, 5))) shouldBe 8
    Day2.part1(List(List(5, 1, 9, 5), List(7, 5, 3), List(2, 4, 6, 8))) shouldBe 18
  }

  "Part 2" should "should use the only whole division" in {
    Day2.part2(List(List(5, 9, 2, 8), List(9, 4, 7, 3), List(3, 8, 6, 5))) shouldBe 9
  }
}
