package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {

  "Part 1" should "find the shortest path" in {
    Day17.part1("ihgpwlah") shouldBe "DDRRRD"
    Day17.part1("kglvqrro") shouldBe "DDUDRLRRUDRD"
    Day17.part1("ulqzkmiv") shouldBe "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
  }

  "Part 2" should "length of the longest path" in {
    Day17.part2("ihgpwlah") shouldBe 370
    Day17.part2("kglvqrro") shouldBe 492
    Day17.part2("ulqzkmiv") shouldBe 830
  }
}
