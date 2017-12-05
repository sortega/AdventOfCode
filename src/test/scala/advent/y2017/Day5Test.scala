package advent.y2017

import advent.y2017.Day5.State
import org.scalatest.{FlatSpec, Matchers}

class Day5Test extends FlatSpec with Matchers {

  "A state" should "increment instructions after execution" in {
    val state = State(0, 3)
    state.step shouldBe Some(State(1, 3))
  }

  it should "do a relative jump with each instruction" in {
    val initial = State(0, 3, 0, 1, -3)
    initial.states shouldBe Stream(
      initial,
      State(1, 3, 0, 1, -3),
      State(Vector(2, 3, 0, 1, -3), next = 1),
      State(Vector(2, 4, 0, 1, -3), next = 4),
      State(Vector(2, 4, 0, 1, -2), next = 1),
      State(Vector(2, 5, 0, 1, -2), next = 5)
    )
  }

  "Part 1" should "count instructions until escaping" in {
    Day5.part1(Vector(0, 3, 0, 1, -3)) shouldBe 5
  }

  "Part 2" should "count instructions until escaping" in {
    Day5.part2(Vector(0, 3, 0, 1, -3)) shouldBe 10
  }
}
