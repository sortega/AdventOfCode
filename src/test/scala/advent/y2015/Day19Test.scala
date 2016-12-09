package advent.y2015

import advent.y2015.Day19.{Configuration, Rule, Rules}
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day19Test extends FlatSpec with ShouldMatchers {

  val configuration =
    """H => HO
      |H => OH
      |O => HH
      |
      |HOH
    """.stripMargin

  "A configuration" should "parsed" in {
    Configuration.parse(configuration) shouldBe Configuration(
      molecule = "HOH",
      rewriteRules = Rules(Set(
        Rule("H", "HO"),
        Rule("H", "OH"),
        Rule("O", "HH")
      ))
    )
  }

  "A rule" should "replace multiple matches" in {
    Rule("H", "HO").apply("HOH") shouldBe Set("HOOH", "HOHO")
  }

  it should "replace overlapping matches" in {
    Rule("HH", "HO").apply("HHH") shouldBe Set("HOH", "HHO")
  }

  "Part 1" should "should count the number of output molecules" in {
    Day19.part1(configuration) shouldBe 4
  }

  "Part 2" should "should count the number of steps to produce a molecule" in {
    Day19.part2(
      """e => H
        |e => O
        |H => HO
        |H => OH
        |O => HH
        |
        |HOHOHO
      """.stripMargin) shouldBe 6
  }
}
