package advent.y2016

import advent.y2016.Day11.{Factory, Floor, Generator, Microchip}
import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {

  val testFactory = Factory(elevator = Floor.F1,
                            floors = Map(
                              Floor.F1 -> Set(Microchip("hydrogen"), Microchip("lithium")),
                              Floor.F2 -> Set(Generator("hydrogen")),
                              Floor.F3 -> Set(Generator("lithium")),
                              Floor.F4 -> Set.empty
                            ))

  "A factory" should "have a nice string representation" in {
    testFactory.toString shouldBe """F4 .  .  .  .  .
                                    |F3 .  .  .  LG .
                                    |F2 .  HG .  .  .
                                    |F1 E  .  HM .  LM
                                    |""".stripMargin
  }

  it should "be able to generate safe transitions" in {
    testFactory.transitions shouldBe Seq(testFactory.moveTo(Floor.F2, Set(Microchip("hydrogen"))))
  }

  "Part 1" should "count the number of steps to completion" in {
    Day11.solve(testFactory) shouldBe 11
  }
}
