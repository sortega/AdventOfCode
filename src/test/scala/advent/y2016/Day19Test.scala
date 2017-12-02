package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day19Test extends FlatSpec with Matchers {

  "Part 1" should "compute the elf taking all" in {
    Day19.part1(1) shouldBe 1
    Day19.part1(2) shouldBe 1
    Day19.part1(3) shouldBe 3
    Day19.part1(4) shouldBe 1
    Day19.part1(5) shouldBe 3
    Day19.part1(6) shouldBe 5
  }

  "Part 2" should "compute the elf taking all" in {
    Day19.part2(1) shouldBe 1
    Day19.part2(2) shouldBe 1
    Day19.part2(3) shouldBe 3
    Day19.part2(4) shouldBe 1
    Day19.part2(5) shouldBe 2
    Day19.part2(6) shouldBe 3
    Day19.part2(7) shouldBe 5
    Day19.part2(8) shouldBe 7
    Day19.part2(9) shouldBe 9
    Day19.part2(10) shouldBe 1
    Day19.part2(11) shouldBe 2
    Day19.part2(12) shouldBe 3
    Day19.part2(13) shouldBe 4
    Day19.part2(14) shouldBe 5
    Day19.part2(15) shouldBe 6
    Day19.part2(16) shouldBe 7
    Day19.part2(17) shouldBe 8
    Day19.part2(18) shouldBe 9
    Day19.part2(19) shouldBe 11
    Day19.part2(20) shouldBe 13
    Day19.part2(21) shouldBe 15
    Day19.part2(22) shouldBe 17
  }
}
