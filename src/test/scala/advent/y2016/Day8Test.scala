package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {

  "Part 1" should "should lit some lights on and off" in {
    Day8.part1(
      Seq("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1"),
      rows = 3,
      cols = 8) shouldBe 6
  }

  "Part 2" should "should print the final screen to string" in {
    Day8.part2(
      Seq("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1"),
      rows = 3,
      cols = 8) shouldBe """.#..#.#.
                           |#.#.....
                           |.#......
                           |""".stripMargin
  }
}
