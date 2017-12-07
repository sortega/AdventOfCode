package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day7Test extends FlatSpec with Matchers {

  private val sampleInput =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
      |""".stripMargin

  "Part 1" should "identify the root id" in {
    Day7.part1(sampleInput) shouldBe "tknk"
  }

  "Part 2" should "tell the new weight after rebalancing" in {
    Day7.part2(sampleInput) shouldBe 60
  }
}
