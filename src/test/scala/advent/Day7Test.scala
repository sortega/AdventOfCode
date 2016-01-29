package advent

import org.scalatest.{FlatSpec, ShouldMatchers}

class Day7Test extends FlatSpec with ShouldMatchers {

  "The circuit parser" should "parse hardwired signals" in {
    ???
  }

  it should "parse binary gate connections" in {
    ???
  }

  it should "parse not gate connections" in {
    ???
  }

  it should "parse shift connections" in {
    ???
  }

  it should "evaluate a simple circuit" in {
    val circuit =
      """123 -> x
        |456 -> y
        |x AND y -> d
        |x OR y -> e
        |x LSHIFT 2 -> f
        |y RSHIFT 2 -> g
        |NOT x -> h
        |NOT y -> i
      """.stripMargin
//    Day7.evaluate(circuit, "d") shouldBe 72
//    Day7.evaluate(circuit, "e") shouldBe 507
//    Day7.evaluate(circuit, "f") shouldBe 492
//    Day7.evaluate(circuit, "g") shouldBe 114
//    Day7.evaluate(circuit, "h") shouldBe 65412
//    Day7.evaluate(circuit, "i") shouldBe 65079
//    Day7.evaluate(circuit, "x") shouldBe 123
//    Day7.evaluate(circuit, "y") shouldBe 456
  }

  "Part 1" should "evaluate hardwired signals" in {
    Day7.part1("1 -> a") shouldBe 1
  }

  it should "evaluate aliased signals" in {
    Day7.part1("1 -> b\nb -> a") shouldBe 1
  }

  it should "evaluate negated signals" in {
    Day7.part1("65535 -> x\nNOT x -> a") shouldBe 0
  }

  it should "evaluate binary operated signals" in {
    Day7.part1("1 -> x\n2 -> y\nx OR y -> a") shouldBe 3
    Day7.part1("1 -> x\n2 -> y\nx AND y -> a") shouldBe 0
  }

  it should "evaluate shifted signals" in {
    Day7.part1("1 -> x\nx LSHIFT 3 -> a") shouldBe 8
    Day7.part1("7 -> x\nx RSHIFT 2 -> a") shouldBe 1
  }

  "Part 2" should "override b with the initial value of a and compute a again" in {
    Day7.part2("1 -> b\nb LSHIFT 1 -> a") shouldBe 4
  }
}
