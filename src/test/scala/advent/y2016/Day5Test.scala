package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day5Test extends FlatSpec with Matchers {

  "Part 1" should "should 'mine' the password" ignore {
    Day5.part1("abc", zeroes = 3) shouldBe "d8d60944"
    Day5.part1("abc") shouldBe "18f47a30"
  }

  "Part 2" should "should 'mine' in a more weird fashion" ignore {
    Day5.part2("abc", zeroes = 3) shouldBe "2fe0c57e"
  }
}
