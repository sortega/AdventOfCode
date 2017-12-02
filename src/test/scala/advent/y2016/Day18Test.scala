package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day18Test extends FlatSpec with Matchers {

  "A trap automaton" should "be arbitrary extensible" in {
    Day18.trapAutomaton("..^^.").take(3).mkString("\n") shouldBe """..^^.
                                                                   |.^^^^
                                                                   |^^..^""".stripMargin
    Day18.trapAutomaton(".^^.^.^^^^").take(10).mkString("\n") shouldBe """.^^.^.^^^^
                                                                         |^^^...^..^
                                                                         |^.^^.^.^^.
                                                                         |..^^...^^^
                                                                         |.^^^^.^^.^
                                                                         |^^..^.^^..
                                                                         |^^^^..^^^.
                                                                         |^..^^^^.^^
                                                                         |.^^^..^.^^
                                                                         |^^.^^^..^^""".stripMargin
  }

  "Day 18" should "count the number of safe tiles" in {
    Day18.countSafeTiles(".^^.^.^^^^", rows = 10) shouldBe 38
  }
}
