package advent

import advent.Day16.{ComplexComparator, SimpleComparator, Evidence, Profile}
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day16Test extends FlatSpec with ShouldMatchers {

  "A profile" should "be parsed" in {
    Profile.parse("Sue 1: goldfish: 9, cars: 0, samoyeds: 9") shouldBe
      Profile(id = 1, Map("goldfish" -> 9, "cars" -> 0, "samoyeds" -> 9))
  }

  "Evidence" should "be parsed" in {
    Evidence.parse("children: 3\ncats: 7\n") shouldBe
      Evidence(Map("children" -> 3, "cats" -> 7))
  }

  "Simple comparator" should "treat evidence as exact inferences" in {
    SimpleComparator.isCompatible("cats", observed = 1, remembered = 1) shouldBe true
    SimpleComparator.isCompatible("children", observed = 1, remembered = 2) shouldBe false
  }

  "Complex comparator" should "treat cats and trees readings as a lower bound" in {
    ComplexComparator.isCompatible("cats", observed = 1, remembered = 0) shouldBe false
    ComplexComparator.isCompatible("cats", observed = 1, remembered = 1) shouldBe false
    ComplexComparator.isCompatible("cats", observed = 1, remembered = 2) shouldBe true
    ComplexComparator.isCompatible("trees", observed = 10, remembered = 9) shouldBe false
    ComplexComparator.isCompatible("trees", observed = 10, remembered = 10) shouldBe false
    ComplexComparator.isCompatible("trees", observed = 10, remembered = 11) shouldBe true
  }

  it should "treat pomeranians and goldfish readings as an upper bound" in {
    ComplexComparator.isCompatible("pomeranians", observed = 1, remembered = 0) shouldBe true
    ComplexComparator.isCompatible("pomeranians", observed = 1, remembered = 1) shouldBe false
    ComplexComparator.isCompatible("pomeranians", observed = 1, remembered = 2) shouldBe false
    ComplexComparator.isCompatible("goldfish", observed = 10, remembered = 9) shouldBe true
    ComplexComparator.isCompatible("goldfish", observed = 10, remembered = 10) shouldBe false
    ComplexComparator.isCompatible("goldfish", observed = 10, remembered = 11) shouldBe false
  }

  it should "treat other measures as exact values" in {
    ComplexComparator.isCompatible("other", observed = 1, remembered = 0) shouldBe false
    ComplexComparator.isCompatible("other", observed = 1, remembered = 1) shouldBe true
    ComplexComparator.isCompatible("other", observed = 1, remembered = 2) shouldBe false
  }

  "Part 1" should "select the first Sue matching all the evidence" in {
    val profiles =
      """Sue 1: goldfish: 9, cars: 0, samoyeds: 9
        |Sue 2: cats: 5, trees: 8, goldfish: 8
        |Sue 3: pomeranians: 2, children: 3, cars: 2
        |Sue 4: samoyeds: 2, children: 3, trees: 5
      """.stripMargin
    val evidence =
      """children: 3
        |cats: 7
        |cars: 2
        |goldfish: 3
        |pomeranians: 2
        |samoyeds: 2
        |trees: 5
      """.stripMargin
    Day16.part1(profiles, evidence) shouldBe 3
  }

  "Part 2" should "select the first Sue matching all the evidence" in {
    val profiles =
      """Sue 1: goldfish: 9, cars: 0, samoyeds: 9
        |Sue 2: cats: 5, trees: 8, goldfish: 8
        |Sue 3: pomeranians: 1, children: 3, cars: 2
        |Sue 4: samoyeds: 2, children: 3, trees: 5
      """.stripMargin
    val evidence =
      """children: 3
        |cats: 7
        |cars: 2
        |goldfish: 3
        |pomeranians: 2
        |samoyeds: 2
        |trees: 5
      """.stripMargin
    Day16.part2(profiles, evidence) shouldBe 3
  }
}
