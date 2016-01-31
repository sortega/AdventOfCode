package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day11Test extends FlatSpec with ShouldMatchers {

  "A password" should "be encoded as a number" in {
    Day11.toNumber("aaaaaaaa") shouldBe 0
    Day11.toNumber("aaaaaaab") shouldBe 1
    Day11.toNumber("aaaaaaba") shouldBe 26
    Day11.toNumber("aaaaaabc") shouldBe 28
    Day11.toNumber("abcdefgh") shouldBe 334123303
  }

  it should "be get from a number" in {
    Day11.toPassword(0) shouldBe "aaaaaaaa"
    Day11.toPassword(1) shouldBe "aaaaaaab"
    Day11.toPassword(26) shouldBe "aaaaaaba"
    Day11.toPassword(28) shouldBe "aaaaaabc"
  }

  it should "have an increasing triplet to be compliant" in {
    Day11.isCompliant("aabcmmmm") shouldBe true
    Day11.isCompliant("aazcmmmm") shouldBe false
  }

  it should "have no i, o or l to be compliant" in {
    Day11.isCompliant("aabcmmmm") shouldBe true
    Day11.isCompliant("aabcimmm") shouldBe false
    Day11.isCompliant("aabcmomm") shouldBe false
    Day11.isCompliant("aabcmmlm") shouldBe false
  }

  it should "have two different, non-overlapping doubled letters to be compliant" in {
    Day11.isCompliant("aabcmmmm") shouldBe true
    Day11.isCompliant("zabcjmmm") shouldBe false
    Day11.isCompliant("zabcjmzm") shouldBe false
  }

  "Part 1" should "find out the next valid password" in {
    Day11.part1("abcdefgh") shouldBe "abcdffaa"
  }

  "Part 2" should "find out the second next valid password" in {
    Day11.part2("abcdefgh") shouldBe "abcdffbb"
  }
}
