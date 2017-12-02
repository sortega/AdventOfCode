package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day1Test extends FlatSpec with Matchers {

  "Part 1" should "count digits followed by the same digit" in {
    Day1.part1("1122") shouldBe 3
    Day1.part1("1111") shouldBe 4
    Day1.part1("1234") shouldBe 0
    Day1.part1("91212129") shouldBe 9
  }

  "Part 2" should "count digits that matches the same one shifted by half the length of the string" in {
    Day1.part2("1212") shouldBe 6
    Day1.part2("1221") shouldBe 0
    Day1.part2("123425") shouldBe 4
    Day1.part2("123123") shouldBe 12
    Day1.part2("12131415") shouldBe 4
  }
}
