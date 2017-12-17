package advent.y2017

import advent.y2017.Day17.CircularBuffer
import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {

  "A circular buffer" should "insert the first element to the right of 0" in {
    CircularBuffer.withStep(3).insert(1) shouldBe CircularBuffer(Vector(0, 1), offset = 1, step = 3)
  }

  it should "insert after moving 'step' positions and to the right" in {
    CircularBuffer(Vector(0, 1), offset = 1, step = 3)
      .insert(2) shouldBe CircularBuffer(Vector(0, 2, 1), offset = 1, step = 3)
    CircularBuffer(Vector(0, 2, 1), offset = 1, step = 3)
      .insert(3) shouldBe CircularBuffer(Vector(0, 2, 3, 1), offset = 2, step = 3)
  }

  "Part 1" should "find the number that lands at the right of 2017" in {
    Day17.part1(3) shouldBe 638
  }

  "Part 2" should "find the element to the right 0 after many more iterations" in {
    Day17.part2(3) shouldBe 41797835
  }
}
