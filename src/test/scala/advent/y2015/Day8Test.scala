package advent.y2015

import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {

  "String decoding" should "decode regular strings" in {
    Day8.decode("\"\"") shouldBe ""
    Day8.decode("\"abc\"") shouldBe "abc"
  }

  it should "decode escaped quotes" in {
    Day8.decode(""""aaa\"aaa"""") shouldBe "aaa\"aaa"
  }

  it should "decode escaped slashes" in {
    Day8.decode(""""aaa\\aaa"""") shouldBe "aaa\\aaa"
  }

  it should "decode ascii escapes" in {
    Day8.decode(""""\x27"""") shouldBe "'"
  }

  "Part 1" should "compute the overhead of the quoted string representation" in {
    Day8.part1(""" "" """) shouldBe 2
    Day8.part1(""" "abc" """) shouldBe 2
    Day8.part1(""" "aaa\"aaa" """) shouldBe 3
    Day8.part1(""" "aaa\\aaa" """) shouldBe 3
    Day8.part1(""" "\x27" """) shouldBe 5
  }

  it should "sum the overhead of multiple quoted strings" in {
    Day8.part1(
      """""
        |"abc"
        |"aaa\"aaa"
        |"\x27" """.stripMargin) shouldBe 12
  }

  "String encoding" should "encode regular strings" in {
    Day8.encode("") shouldBe "\"\""
    Day8.encode("abc") shouldBe "\"abc\""
  }

  it should "encode escaped quotes" in {
    Day8.encode("aaa\"aaa") shouldBe """"aaa\"aaa""""
  }

  it should "encode escaped slashes" in {
    Day8.encode("aaa\\aaa") shouldBe """"aaa\\aaa""""
  }

  "Part 2" should "compute the overhead of escaping the string representation" in {
    Day8.part2(""" "" """) shouldBe 4
    Day8.part2(""" "abc" """) shouldBe 4
    Day8.part2(""" "aaa\"aaa" """) shouldBe 6
    Day8.part2(""" "\x27" """) shouldBe 5
  }
}
