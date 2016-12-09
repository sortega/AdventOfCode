package advent.y2015

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day13Test extends FlatSpec with ShouldMatchers {

  "Person preferences" should "be parsed from string" in {
    Day13.Preferences.parse(
      """Alice would lose 57 happiness units by sitting next to Bob.
        |Kate would gain 71 happiness units by sitting next to Eric.
      """.stripMargin) shouldBe Map(
      ("Alice", "Bob") -> -57,
      ("Kate", "Eric") -> 71
    )
  }

  val sampleInput = """Alice would gain 54 happiness units by sitting next to Bob.
                      |Alice would lose 79 happiness units by sitting next to Carol.
                      |Alice would lose 2 happiness units by sitting next to David.
                      |Bob would gain 83 happiness units by sitting next to Alice.
                      |Bob would lose 7 happiness units by sitting next to Carol.
                      |Bob would lose 63 happiness units by sitting next to David.
                      |Carol would lose 62 happiness units by sitting next to Alice.
                      |Carol would gain 60 happiness units by sitting next to Bob.
                      |Carol would gain 55 happiness units by sitting next to David.
                      |David would gain 46 happiness units by sitting next to Alice.
                      |David would lose 7 happiness units by sitting next to Bob.
                      |David would gain 41 happiness units by sitting next to Carol.
                    """.stripMargin

  "Part 1" should "determine the optimum arrangement" in {
    Day13.part1(sampleInput) shouldBe 330
  }

  "Part 2" should "determine the optimum arrangement including yourself" in {
    Day13.part2(sampleInput) shouldBe 286
  }
}
