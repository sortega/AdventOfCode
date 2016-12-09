package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day12Test extends FlatSpec with ShouldMatchers {

  "Part 1" should "sum all numbers in input" in {
    Day12.part1("[]") shouldBe 0
    Day12.part1("[1,2,3]") shouldBe 6
    Day12.part1("{\"a\":{\"b\":4},\"c\":-1}") shouldBe 3
    Day12.part1("{\"a\":[-1,1]}") shouldBe 0
    Day12.part1("[-1,{\"a\":1}]") shouldBe 0
  }

  "Part 2" should "sum all numbers not in red objects" in {
    Day12.part2("[1,2,3]") shouldBe 6
    Day12.part2("[1,{\"c\":\"red\",\"b\":2},3]") shouldBe 4
    Day12.part2("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") shouldBe 0
    Day12.part2("[1,\"red\",5]") shouldBe 6
  }
}
