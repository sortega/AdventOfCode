package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "Part 1" should "count possible triangles" in {
    Day3.part1(Seq.empty) shouldBe 0
    Day3.part1(Seq("10 10 10")) shouldBe 1
    Day3.part1(Seq("5 10 25", "10 10 10")) shouldBe 1
  }

  "Part 2" should "count possible triangles listed by columns" in {
    Day3.part2(Seq.empty) shouldBe 0
    Day3.part2(Seq("10 10 25", "10  5 10", "10  5 16")) shouldBe 2
  }
}
