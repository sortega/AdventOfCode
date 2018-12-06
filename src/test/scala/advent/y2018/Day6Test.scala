package advent.y2018

import advent.shared.geom.Point
import advent.shared.test.UnitTest

final class Day6Test extends UnitTest {

  private val sampleInput = List(
    Point(1, 1),
    Point(1, 6),
    Point(8, 3),
    Point(3, 4),
    Point(5, 5),
    Point(8, 9)
  )

  "Part 1" should "find size of the biggest finite region" in {
    Day6.part1(sampleInput) should ===(17)
  }

  "Part 2" should "the size of the region with max dist 32 to all targets" in {
    Day6.part2(sampleInput, maxDist = 32) should ===(16)
  }
}
