package advent.y2017

import advent.y2017.Day6.MemoryBank
import org.scalatest.{FlatSpec, Matchers}

class Day6Test extends FlatSpec with Matchers {

  "A memory bank" should "redistribute the bank with most blocks" in {
    MemoryBank(0, 2, 7, 0).redistribute shouldBe MemoryBank(2, 4, 1, 2)
    MemoryBank(2, 4, 1, 2).redistribute shouldBe MemoryBank(3, 1, 2, 3)
  }

  it should "break ties using the lower index number" in {
    MemoryBank(3, 1, 2, 3).redistribute shouldBe MemoryBank(0, 2, 3, 4)
  }

  "Part 1" should "tell how many redistributions are before getting an unseen configuration" in {
    Day6.part1(Vector(0, 2, 7, 0)) shouldBe 5
  }

  "Part 2" should "measure the length of the cycle in transitions" in {
    Day6.part2(Vector(0, 2, 7, 0)) shouldBe 4
  }
}
