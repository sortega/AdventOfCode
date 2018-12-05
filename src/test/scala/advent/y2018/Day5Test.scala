package advent.y2018

import advent.shared.test.UnitTest

final class Day5Test extends UnitTest {

  private val sampleInput = "dabAcCaCBAcCcaDA"

  "Part 1" should "return the size of the reduced polymer" in {
    Day5.reduce(sampleInput) should ===("dabCBAcaDA")
    Day5.part1(sampleInput) should ===(10)
  }

  "Part 2" should "get the shortest size removing any one type of element" in {
    Day5.part2(sampleInput) should ===(4)
  }
}
