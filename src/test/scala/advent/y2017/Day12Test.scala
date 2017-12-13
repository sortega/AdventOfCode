package advent.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {

  val sampleGraph =
    """0 <-> 2
      |1 <-> 1
      |2 <-> 0, 3, 4
      |3 <-> 2, 4
      |4 <-> 2, 3, 6
      |5 <-> 6
      |6 <-> 4, 5
      |""".stripMargin

  "Part 1" should "count the size of the connected component of node 0" in {
    Day12.part1(sampleGraph) shouldBe 6
  }

  "Part 2" should "find the number of connected components" in {
    Day12.part2(sampleGraph) shouldBe 2
  }
}
