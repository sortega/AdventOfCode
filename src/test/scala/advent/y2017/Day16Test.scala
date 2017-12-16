package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {

  val input = "s1,x3/4,pe/b"

  "Part 1" should "make programs dance" in {
    Day16.part1(input, size = 5) shouldBe "baedc"
  }

  "Part 2" should "make them dance harder" in {
    Day16.part2(input, size = 5) shouldBe "cbaed"
  }
}
