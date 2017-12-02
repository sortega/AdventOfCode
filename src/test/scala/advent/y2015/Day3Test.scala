package advent.y2015

import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "Part 1" should "track how many houses are visited" in {
    Day3.part1("") shouldBe 1
    Day3.part1(">") shouldBe 2
    Day3.part1("^>v<") shouldBe 4
  }

  it should "not count the same house twice" in {
    Day3.part1("^v^v^v^v^v") shouldBe 2
  }

  "Part 2" should "consider Santa and Robo-Santa taking turns" in {
    Day3.part2("^v") shouldBe 3
    Day3.part2("^>v<") shouldBe 3
    Day3.part2("^v^v^v^v^v") shouldBe 11
  }
}
