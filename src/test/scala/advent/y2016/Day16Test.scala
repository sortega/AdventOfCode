package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {

  "Day 16" should "compute a bizarre checksum" in {
    Day16.solve("10000", 20) shouldBe "01100"
  }
}
