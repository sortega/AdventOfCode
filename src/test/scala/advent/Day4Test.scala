package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day4Test extends FlatSpec with ShouldMatchers {

  "Advent coin miner" should "mine with difficulty 1" in {
    Day4.mineAdventCoin("abcdef", 1) shouldBe 31
  }

  it should "mine with difficulty 2" in {
    Day4.mineAdventCoin("abcdef", 2) shouldBe 298
  }

  it should "mine with difficulty 3" in {
    Day4.mineAdventCoin("abcdef", 3) shouldBe 3337
  }
}
