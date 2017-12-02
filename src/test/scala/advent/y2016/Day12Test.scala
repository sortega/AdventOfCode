package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {

  "Part 1" should "return the final value of the a register" in {
    Day12.part1("""cpy 41 a
                  |inc a
                  |inc a
                  |dec a
                  |jnz a 2
                  |dec a
                  |""".stripMargin) shouldBe 42
  }

  "Part 2" should "do the same but with c=1 at the start" in {
    Day12.part2("""cpy c a""") shouldBe 1
  }
}
