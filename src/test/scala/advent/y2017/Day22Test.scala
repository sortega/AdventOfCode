package advent.y2017

import advent.shared.geom.Point
import advent.shared.geom.CardinalPoint.{North, West}
import advent.y2017.Day22.{Carrier, SimpleWorld}
import org.scalatest.{FlatSpec, Matchers}

class Day22Test extends FlatSpec with Matchers {

  val world       = SimpleWorld(Carrier.Initial, infected = Set(Point(-1, 0), Point(1, 1)))
  val sampleInput = """..#
                      |#..
                      |...
                      |""".stripMargin

  "World" should "get updated in carrier bursts" in {
    world.burst shouldBe SimpleWorld(Carrier(Point(-1, 0), West),
                               infected = Set(Point(-1, 0), Point.Origin, Point(1, 1)))
    world.burst.burst shouldBe SimpleWorld(Carrier(Point(-1, 1), North),
                                     infected = Set(Point.Origin, Point(1, 1)))
  }

  it should "be parsed" in {
    SimpleWorld.parse(sampleInput) shouldBe world
  }

  "Part 1" should "count infections after a number of bursts" in {
    Day22.part1(sampleInput) shouldBe 5587
  }

  "Part 2" should "count infections with more complex rules" in {
    Day22.part2(sampleInput) shouldBe 2511944
  }
}
