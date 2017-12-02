package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day2Test extends FlatSpec with Matchers {

  "Part 1" should "punch no keys for empty instructions" in {
    Day2.part1("") shouldBe ""
  }

  it should "punch '5' once per line if the focus don't move" in {
    Day2.part1("\n") shouldBe "5"
    Day2.part1("\n\n") shouldBe "55"
  }

  it should "move up until hitting the upper keypad limit" in {
    Day2.part1("U\n") shouldBe "2"
    Day2.part1("UU\n") shouldBe "2"
    Day2.part1("UUU\n") shouldBe "2"
    Day2.part1("UUU\nU\n") shouldBe "22"
  }

  it should "move elsewhere until hitting the keypad limits" in {
    Day2.part1("ULL\nRRDDD\nLURDL\nUUUUD\n") shouldBe "1985"
  }

  "Part 2" should "do the same with a different keypad layout" in {
    Day2.part2("ULL\nRRDDD\nLURDL\nUUUUD\n") shouldBe "5DB3"
  }
}
