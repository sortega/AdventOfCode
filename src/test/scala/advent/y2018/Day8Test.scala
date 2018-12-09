package advent.y2018

import advent.y2018.Day8.Tree
import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {

  private val input = Tree.parse("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  "Part 1" should "sum tree metadata" in {
    Day8.part1(input) should ===(138)
  }

  "Part 2" should "the value of the root node" in {
    Day8.part2(input) should ===(66)
  }
}
