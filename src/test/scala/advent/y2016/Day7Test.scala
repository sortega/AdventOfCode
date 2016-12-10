package advent.y2016

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day7Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "count the number of ABBA-compliant strings" in {
    Day7.part1(Seq("abba[mnop]qrst")) shouldBe 1
    Day7.part1(Seq("abcd[bddb]xyyx")) shouldBe 0
    Day7.part1(Seq("aaaa[qwer]tyui")) shouldBe 0
    Day7.part1(Seq("ioxxoj[asdfgh]zxcvbn")) shouldBe 1
  }

  "Part 2" should "count even more bizarre rules" in {
    Day7.part2(Seq("aba[bab]xyz")) shouldBe 1
    Day7.part2(Seq("xyx[xyx]xyx")) shouldBe 0
    Day7.part2(Seq("aaa[kek]eke")) shouldBe 1
    Day7.part2(Seq("zazbz[bzb]cdb")) shouldBe 1
  }
}
