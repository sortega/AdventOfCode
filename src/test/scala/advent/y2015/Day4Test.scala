package advent.y2015

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day4Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "mine advent coins!" ignore {
    Day4.part1("abcdef") shouldBe 609043
  }

  "Part 2" should "mine with one more zero" ignore {
    Day4.part2("abcdef") shouldBe 6742839
  }
}
