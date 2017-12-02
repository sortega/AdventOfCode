package advent.y2015

import org.scalatest.{FlatSpec, Matchers}

class Day5Test extends FlatSpec with Matchers {

  "Part 1" should "consider nice strings with 3 vowels and a double letter" in {
    Day5.part1("ugknbfddgicrmopn") shouldBe 1
    Day5.part1("aaa") shouldBe 1
  }

  it should "consider naughty strings without doubled letters" in {
    Day5.part1("jchzalrnumimnmhp") shouldBe 0
  }

  it should "consider naughty strings containing xy" in {
    Day5.part1("haegwjzuvuyypxyu") shouldBe 0
  }

  it should "consider naughty strings containing less than 3 vowels" in {
    Day5.part1("dvszwmarrgswjxmb") shouldBe 0
  }

  "Part 2" should "consider strings with repeated pairs and 3-palindromes nice" in {
    Day5.part2("qjhvhtzxzqqjkmpb\nxxyxx") shouldBe 2
  }

  it should "consider strings without repeated pairs naughty" in {
    Day5.part2("ieodomkazucvgmuy") shouldBe 0
  }
}
