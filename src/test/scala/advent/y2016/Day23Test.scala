package advent.y2016

import advent.y2016.AssemblyBunny._
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day23Test extends FlatSpec with ShouldMatchers {

  private val initialListing =
    """cpy 2 a
      |tgl a
      |tgl a
      |tgl a
      |cpy 1 a
      |dec a
      |dec a
      |""".stripMargin

  "Assemblybunny" should "support the TGL instruction" in {
    val finalState = Computer(AssemblyGrammar.parse(initialListing)).run
    finalState.registers(Register.A) shouldBe 3
  }

  "Part 1" should "initialize register a with 7 and take the final value in a" in {
    Day23.part1(initialListing) shouldBe 3
  }
}
