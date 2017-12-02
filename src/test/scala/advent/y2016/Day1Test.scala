package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day1Test extends FlatSpec with Matchers {

  "Part 1" should "have distance 0 for empty instructions" in {
    Day1.part1("") shouldBe 0
    Day1.part1(" ") shouldBe 0
  }

  it should "have distance k for a single-step instruction of length k" in {
    Day1.part1("R1") shouldBe 1
    Day1.part1("L1") shouldBe 1
    Day1.part1("L3") shouldBe 3
    Day1.part1("R7") shouldBe 7
  }

  it should "compute distance for multiple-step instructions" in {
    Day1.part1("R2, L3") shouldBe 5
    Day1.part1("R2, R2, R2") shouldBe 2
    Day1.part1("R5, L5, R5, R3") shouldBe 12
  }

  "Part 2" should "compute the distance to the first location visited twice" in {
    Day1.part2("R8, R4, R4, R8") shouldBe 4
  }
}
