package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {

  val listing =
    """b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10
      |""".stripMargin

  "Part 1" should "return the biggest value at the end" in {
    Day8.part1(listing) shouldBe 1
  }

  "Part 2" should "return the biggest value along the states" in {
    Day8.part2(listing) shouldBe 10
  }
}
