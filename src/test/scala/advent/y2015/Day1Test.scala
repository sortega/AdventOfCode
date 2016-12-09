package advent.y2015

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day1Test extends FlatSpec with ShouldMatchers {

  "Part 1 floor tracking" should "get to the 0 floor for the empty input" in {
    Day1.part1("") shouldBe 0
  }

  it should "get to higher floors by (" in {
    Day1.part1("(") shouldBe 1
    Day1.part1("((") shouldBe 2
  }

  it should "go down with )" in {
    Day1.part1("(())") shouldBe 0
    Day1.part1("()()") shouldBe 0
    Day1.part1("))(((((") shouldBe 3
  }

  it should "be able to go to basement plants" in {
    Day1.part1("())") shouldBe -1
    Day1.part1("))(") shouldBe -1
    Day1.part1(")))") shouldBe -3
    Day1.part1(")())())") shouldBe -3
  }

  "Part 2 floor tracking" should "detect when the basement is hit" in {
    Day1.part2(")") shouldBe 1
    Day1.part2("()())") shouldBe 5
  }
}
