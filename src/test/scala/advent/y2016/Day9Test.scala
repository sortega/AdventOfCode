package advent.y2016

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day9Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "compute uncompressed text size" in {
    Day9.part1("") shouldBe 0
    Day9.part1("ADVENT") shouldBe 6
    Day9.part1("A(1x5)BC") shouldBe 7
    Day9.part1("A(2x2)BCD(2x2)EFG") shouldBe 11
    Day9.part1("(6x1)(1x3)A") shouldBe 6
    Day9.part1("X(8x2)(3x3)ABCY") shouldBe 18
  }

  "Part 2" should "consider a recursive compression scheme" in {
    Day9.part2("") shouldBe 0
    Day9.part2("ADVENT") shouldBe 6
    Day9.part2("(3x3)XYZ") shouldBe 9
    Day9.part2("X(8x2)(3x3)ABCY") shouldBe 20
    Day9.part2("(27x12)(20x12)(13x14)(7x10)(1x12)A") shouldBe 241920
    Day9.part2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") shouldBe 445
  }
}
