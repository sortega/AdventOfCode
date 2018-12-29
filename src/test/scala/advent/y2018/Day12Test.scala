package advent.y2018

import advent.y2018.Day12.{Generation, RuleSet}
import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {

  private val gen0    = Generation.parse("#..#.#..##......###...###")
  private val ruleSet = RuleSet.parse("""...## => #
                                        |..#.. => #
                                        |.#... => #
                                        |.#.#. => #
                                        |.#.## => #
                                        |.##.. => #
                                        |.#### => #
                                        |#.#.# => #
                                        |#.### => #
                                        |##.#. => #
                                        |##.## => #
                                        |###.. => #
                                        |###.# => #
                                        |####. => #
                                        |""".stripMargin.lines.toList)

  "Part 1" should "compute 20 generations" in {
    Day12.part1(ruleSet, gen0) shouldBe 325
  }

  "Part 2" should "???" in {}
}
