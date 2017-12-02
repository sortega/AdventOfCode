package advent.y2016

import advent.y2016.Day15.Disc
import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {

  "Day 15" should "find the first time discs are aligned" in {
    val discs = Seq(
      Disc(1, positions = 5, startsAt = 4),
      Disc(2, positions = 2, startsAt = 1)
    )
    Day15.apply(discs) shouldBe 5
  }
}
