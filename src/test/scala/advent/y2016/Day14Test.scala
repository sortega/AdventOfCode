package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {

  "Part 1" should "find the 64th valid key index" in {
    Day14.part1("abc") shouldBe 22728
  }

  "Part 2" should "do the same having 2016 rounds of md5 per key" in {
    Day14.part2("abc") shouldBe 22551
  }
}
