package advent.y2017

import advent.shared.geom.Point
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "Spiral coordinates" should "flow into a stream" in {
    Day3.spiralCoords.take(10) shouldBe List(Point(0, 0),
                                             Point(1, 0),
                                             Point(1, 1),
                                             Point(0, 1),
                                             Point(-1, 1),
                                             Point(-1, 0),
                                             Point(-1, -1),
                                             Point(0, -1),
                                             Point(1, -1),
                                             Point(2, -1))
  }

  "Part 1" should "get the Manhattan distance to the point" in {
    Day3.part1(1) shouldBe 0
    Day3.part1(12) shouldBe 3
    Day3.part1(23) shouldBe 2
    Day3.part1(1024) shouldBe 31
  }

  "Part 2" should "get the first value bigger than the input" in {
    Day3.part2(700) shouldBe 747
  }
}
