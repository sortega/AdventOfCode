package advent.y2015

import advent.y2015.Day23.Assembly
import advent.y2015.Day23.Instruction._
import advent.y2015.Day23.Register._
import org.scalatest.{FlatSpec, Matchers}

class Day23Test extends FlatSpec with Matchers {

  "Assembly" should "be parsed" in {
    Assembly.parse("""inc a
                     |jio a, +2
                     |tpl a
                     |inc a
                     |""".stripMargin) shouldBe List(
      INC(A),
      JIO(A, 2),
      TPL(A),
      INC(A)
    )
  }

  "Part 1" should "work" in {
    Day23.part1(
      """inc b
        |jio b, +2
        |tpl b
        |inc b
      """.stripMargin) shouldBe 2
  }

  "Part 2" should "start with a=1" in {
    Day23.part2(
      """inc b
        |jio a, +2
        |tpl b
        |inc b
      """.stripMargin) shouldBe 2
  }
}
