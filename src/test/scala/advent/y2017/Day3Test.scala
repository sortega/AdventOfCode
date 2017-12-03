package advent.y2017

import advent.geom.Point
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "Spiral coordinates" should "be transformed to points" in {
    Day3.coordsFor(1) shouldBe Point.Origin
    Day3.coordsFor(2) shouldBe Point(1, 0)
    Day3.coordsFor(9) shouldBe Point(1, -1)
    Day3.coordsFor(21) shouldBe Point(-2, -2)
    Day3.coordsFor(26) shouldBe Point(3, -2)
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
