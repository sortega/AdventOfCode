package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day6Test extends FlatSpec with Matchers {

  private val sampleInput =
    """eedadn
      |drvtee
      |eandsr
      |raavrd
      |atevrs
      |tsrnev
      |sdttsa
      |rasrtv
      |nssdts
      |ntnada
      |svetve
      |tesnvt
      |vntsnd
      |vrdear
      |dvrsen
      |enarar
      |""".stripMargin

  "Part 1" should "take the most common letter per column" in {
    Day6.part1("") shouldBe empty
    Day6.part1(sampleInput) shouldBe "easter"
  }

  "Part 2" should "take the least common letter per column" in {
    Day6.part2(sampleInput) shouldBe "advent"
  }
}
