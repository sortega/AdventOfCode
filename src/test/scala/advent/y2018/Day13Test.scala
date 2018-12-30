package advent.y2018

import advent.shared.geom.Point
import advent.y2018.Day13.State
import org.scalatest.{FlatSpec, Matchers}

final class Day13Test extends FlatSpec with Matchers {

  private val initialState = State.parse("""/->-\
                                           ||   |  /----\
                                           || /-+--+-\  |
                                           || | |  | v  |
                                           |\-+-/  \-+--/
                                           |  \------/
                                           |""".stripMargin.lines.toList)

  "A state" should "be converted to string" in {
    initialState.toString shouldBe """ 0000000000111
                                     | 0123456789012
                                     |0/->-\
                                     |1|   |  /----\
                                     |2| /-+--+-\  |
                                     |3| | |  | v  |
                                     |4\-+-/  \-+--/
                                     |5  \------/
                                     |""".stripMargin
  }

  it should "step to the next state" in {
    initialState.tick.toString shouldBe """ 0000000000111
                                          | 0123456789012
                                          |0/-->\
                                          |1|   |  /----\
                                          |2| /-+--+-\  |
                                          |3| | |  | |  |
                                          |4\-+-/  \->--/
                                          |5  \------/
                                          |""".stripMargin
  }

  it should "detect crashes" in {
    val states = List.iterate(initialState, 15)(_.tick)
    states.init.foreach { state =>
      state.crashed shouldBe false
    }
    states.last.crashed shouldBe true
    states.last.toString shouldBe """ 0000000000111
                                    | 0123456789012
                                    |0/---\
                                    |1|   |  /----\
                                    |2| /-+--+-\  |
                                    |3| | |  X |  |
                                    |4\-+-/  \-+--/
                                    |5  \------/
                                    |""".stripMargin
  }

  "Part 1" should "produce coordinates of first crash" in {
    Day13.part1(initialState) shouldBe Point(7, 3)
  }

  "Part 2" should "find the position of the last uncrashed cart" in {
    val state = State.parse("""/>-<\
                              ||   |
                              || /<+-\
                              || | | v
                              |\>+</ |
                              |  |   ^
                              |  \<->/
                              |""".stripMargin.lines.toList)
    Day13.part2(state) shouldBe Point(6, 4)
  }
}
