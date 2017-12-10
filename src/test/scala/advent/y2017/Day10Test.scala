package advent.y2017

import advent.y2017.Day10.Cycle
import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {

  "A cycle" should "updated with pinches and twists" in {
    val steps = Vector(3, 4, 1, 5).scanLeft(Cycle.withLength(5))(_.pinchAndTwist(_)).map(_.toVector)
    steps.head shouldBe Vector(0, 1, 2, 3, 4)
    steps(1) shouldBe Vector(2, 1, 0, 3, 4)
    steps(2) shouldBe Vector(4, 3, 0, 1, 2)
    steps(3) shouldBe Vector(4, 3, 0, 1, 2)
    steps(4) shouldBe Vector(3, 4, 2, 1, 0)
  }

  "Part 1" should "pinch and twist and then multiply the final two first numbers" in {
    Day10.part1(Vector(3, 4, 1, 5), length = 5) shouldBe 12
  }

  "Part 2" should "do an amazing number of operations to get a hex hash" in {
    Day10.part2("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    Day10.part2("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    Day10.part2("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    Day10.part2("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}
