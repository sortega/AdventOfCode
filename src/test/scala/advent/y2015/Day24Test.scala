package advent.y2015

import org.scalatest.{FlatSpec, Matchers}

class Day24Test extends FlatSpec with Matchers {

  "Listed subsets" should "be empty for the empty set" in {
    Day24.subsetsInAscendingSize(Set.empty) shouldBe 'empty
  }

  it should "be the identity function for the singleton set" in {
    Day24.subsetsInAscendingSize(Set(1)) shouldBe Seq(Set(1))
  }

  it should "be sorted by size" in {
    Day24.subsetsInAscendingSize(Set(1, 2)) shouldBe Seq(
      Set(1),
      Set(2),
      Set(1, 2)
    )
    Day24.subsetsInAscendingSize(Set(1, 2, 3)) shouldBe Seq(
      Set(1),
      Set(2),
      Set(3),
      Set(1, 2),
      Set(1, 3),
      Set(2, 3),
      Set(1, 2, 3)
    )
  }

  "A set" should "be checked for division into two same-weight subsets" in {
    Day24.isDivisibleIntoEqualParts(Set(1), 2) shouldBe false
    Day24.isDivisibleIntoEqualParts(Set(1, 2, 3), 2) shouldBe true
    Day24.isDivisibleIntoEqualParts((1 to 7).toSet, 2) shouldBe true
  }

  it should "be checked for division into three same-weight subsets" in {
    Day24.isDivisibleIntoEqualParts(Set(1), 3) shouldBe false
    Day24.isDivisibleIntoEqualParts(Set(1, 2, 3), 3) shouldBe false
    Day24.isDivisibleIntoEqualParts(Set(10, 5,9, 3, 2, 1, 8, 7), 3) shouldBe true
  }

  val input = ((1 to 5) ++ (7 to 11)).toSet

  "Part 1" should "find the optimum balance" in {
    Day24.part1(input) shouldBe 99
  }

  "Part 2" should "find the optimum balance considering the trunk" in {
    Day24.part2(input) shouldBe 44
  }
}
