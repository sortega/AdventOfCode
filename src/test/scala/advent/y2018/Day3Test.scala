package advent.y2018

import advent.shared.test.UnitTest
import advent.y2018.Day3._

class Day3Test extends UnitTest {

  val sampleRawInput = """#1 @ 1,3: 4x4
                         |#2 @ 3,1: 4x4
                         |#3 @ 5,5: 2x2
                         |""".stripMargin

  val sampleInput = sampleRawInput.lines.map(Claim.parse).toList

  "A claim" should "know when it intersects another claim" in {
    val List(claim1, claim2, claim3) = sampleInput
    claim1.intersects(claim2) shouldBe true
    claim1.intersects(claim3) shouldBe false
  }

  "Part 1" should "get the size of the overlapping area" in {
    Day3.part1(sampleInput) should ===(4)
  }

  "Part 2" should "find the non-overlapping claim" in {
    Day3.part2(sampleInput) should ===(3)
  }
}
