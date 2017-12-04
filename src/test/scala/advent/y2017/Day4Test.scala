package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day4Test extends FlatSpec with Matchers {

  "Part 1" should "consider valid passphrases without repeated words" in {
    Day4.part1(
      List(
        "aa bb cc dd ee",
        "aa bb cc dd aa",
        "aa bb cc dd aaa"
      )) shouldBe 2
  }

  "Part 2" should "reject passphrases with anagrams" in {
    Day4.part2(
      List(
        "abcde fghij",
        "abcde xyz ecdab",
        "a ab abc abd abf abj",
        "iiii oiii ooii oooi oooo",
        "oiii ioii iioi iiio"
      )) shouldBe 3
  }
}
