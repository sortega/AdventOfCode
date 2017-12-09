package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day9Test extends FlatSpec with Matchers {

  "Part 1" should "sum the score of all groups" in {
    Day9.part1("{}") shouldBe 1
    Day9.part1("{{{}}}") shouldBe (1 + 2 + 3)
    Day9.part1("{{},{}}") shouldBe (1 + 2 + 2)
    Day9.part1("{{{},{},{{}}}}") shouldBe (1 + 2 + 3 + 3 + 3 + 4)
    Day9.part1("{<a>,<a>,<a>,<a>}") shouldBe 1
    Day9.part1("{{<ab>},{<ab>},{<ab>},{<ab>}}") shouldBe (1 + 2 + 2 + 2 + 2)
    Day9.part1("{{<!!>},{<!!>},{<!!>},{<!!>}}") shouldBe (1 + 2 + 2 + 2 + 2)
    Day9.part1("{{<a!>},{<a!>},{<a!>},{<ab>}}") shouldBe (1 + 2)
  }

  "Part 2" should "???" in {
    Day9.part2("<>") shouldBe 0
    Day9.part2("<random characters>") shouldBe 17
    Day9.part2("<<<<>") shouldBe 3
    Day9.part2("<{!>}>") shouldBe 2
    Day9.part2("<!!>") shouldBe 0
    Day9.part2("<!!!>>") shouldBe 0
    Day9.part2("<{o\"i!a,<{i<a>") shouldBe 10
  }
}
