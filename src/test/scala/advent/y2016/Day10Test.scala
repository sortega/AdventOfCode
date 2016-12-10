package advent.y2016

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day10Test extends FlatSpec with ShouldMatchers {

  val sampleCircuit =
    """value 5 goes to bot 2
      |bot 2 gives low to bot 1 and high to bot 0
      |value 3 goes to bot 1
      |bot 1 gives low to output 1 and high to bot 0
      |bot 0 gives low to output 2 and high to output 0
      |value 2 goes to bot 2
      |""".stripMargin

  "Part 1" should "simulate some circuit switching" in {
    Day10.part1(sampleCircuit, numbers = Vector(2, 5)) shouldBe 2
  }

  "Part 2" should "multiply outputs 0, 1 and 2" in {
    Day10.part2(sampleCircuit) shouldBe 30
  }
}
