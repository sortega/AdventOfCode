package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day13Test extends FlatSpec with Matchers {

  val sampleInput = """0: 3
                      |1: 2
                      |4: 4
                      |6: 4
                      |""".stripMargin

  "Part 1" should "determine the severity" in {
    Day13.part1(sampleInput) shouldBe 24
  }

  "Part 2" should "determine the delay for not being caught" in {
    Day13.part2(sampleInput) shouldBe 10
  }
}
