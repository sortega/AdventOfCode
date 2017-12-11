package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {

  "Part 1" should "find the shortest path to where the program lands" in {
    Day11.part1("ne,ne,ne") shouldBe 3
    Day11.part1("ne,ne,sw,sw") shouldBe 0
    Day11.part1("ne,ne,s,s") shouldBe 2
    Day11.part1("se,sw,se,sw,sw") shouldBe 3
  }

  "Part 2" should "find the furthest distance within the path" in {
    Day11.part2("ne,ne,ne") shouldBe 3
    Day11.part2("ne,ne,sw,sw") shouldBe 2
    Day11.part2("ne,ne,s,s") shouldBe 2
    Day11.part2("se,sw,se,sw,sw") shouldBe 3
  }
}
