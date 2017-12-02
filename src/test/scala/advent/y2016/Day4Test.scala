package advent.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day4Test extends FlatSpec with Matchers {

  "Part 1" should "return the sum of valid room ids" in {
    Day4.part1(
      """aaaaa-bbb-z-y-x-123[abxyz]
        |a-b-c-d-e-f-g-h-987[abcde]
        |not-a-real-room-404[oarel]
        |totally-real-room-200[decoy]
        |""".stripMargin) shouldBe (123 + 404 + 987)
  }

  "Part 2" should "return the id of the encrypted 'North Pole objects'" in {
    Day4.part2(
      """aaaaa-bbb-z-y-x-123[abxyz]
        |a-b-c-d-e-f-g-h-987[abcde]
        |northpole-object-storage-0[oebce]
        |not-a-real-room-404[oarel]
        |totally-real-room-200[decoy]
        |""".stripMargin) shouldBe 0
  }
}
