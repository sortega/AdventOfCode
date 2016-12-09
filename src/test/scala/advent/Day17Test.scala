package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day17Test extends FlatSpec with ShouldMatchers {

  val amount = 25
  val containers = List(20, 15, 10, 5, 5)

  "Part 1" should "count how many ways to store eggnog are" in {
    Day17.part1(amount, containers) shouldBe 4
  }

  "Part 2" should "count how many ways to store eggnog are with a minimum of containers" in {
    Day17.part2(amount, containers) shouldBe 3
  }
}
