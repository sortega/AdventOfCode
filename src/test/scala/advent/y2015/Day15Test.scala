package advent.y2015

import advent.y2015.Day15.Ingredient
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day15Test extends FlatSpec with ShouldMatchers {

  "Breaking an amount in all possible partitions" should "produce an empty partition for 0 buckets" in {
    Day15.allPartitions(total = 0, buckets = 0) shouldBe Seq(Vector.empty)
  }

  it should "produce a vector of zeroes when total is zero" in {
    Day15.allPartitions(total = 0, buckets = 1) shouldBe Seq(Vector(0))
    Day15.allPartitions(total = 0, buckets = 2) shouldBe Seq(Vector(0, 0))
  }

  it should "produce all permutations adding up to a general total" in {
    Day15.allPartitions(total = 1, buckets = 1) shouldBe Seq(Vector(1))
    Day15.allPartitions(total = 1, buckets = 2) shouldBe Seq(
      Vector(0, 1),
      Vector(1, 0)
    )
    Day15.allPartitions(total = 2, buckets = 2) shouldBe Seq(
      Vector(0, 2),
      Vector(1, 1),
      Vector(2, 0)
    )
  }

  "An ingredient" should "be parsed" in {
    Ingredient.parse("Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5") shouldBe
      Ingredient(Map("capacity" -> 4, "durability" -> -2, "flavor" -> 0, "texture" -> 0, "calories" -> 5))
  }

  it should "be scaled" in {
    Ingredient(Map("foo" -> 1, "bar" -> 2)) * 2 shouldBe Ingredient(Map("foo" -> 2, "bar" -> 4))
  }

  it should "be added" in {
    Ingredient(Map("foo" -> 1, "bar" -> 2)) + Ingredient(Map("bar" -> 3, "buzz" -> 10)) shouldBe
      Ingredient(Map("foo" -> 1, "bar" -> 5, "buzz" -> 10))
  }

  it should "compute its score" in {
    Ingredient(Map("foo" -> 2, "bar" -> 3)).score shouldBe 6
  }

  it should "ignore calories for the score" in {
    Ingredient(Map("foo" -> 2, "calories" -> 3)).score shouldBe 2
  }

  it should "consider negative properties zero for the score computation" in {
    Ingredient(Map("foo" -> -2, "bar" -> 3)).score shouldBe 0
  }

  val sampleIngredients =
    """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
    """.stripMargin

  "Part 1" should "find the score of the best recipe" in {
    Day15.part1(sampleIngredients) shouldBe 62842880
  }

  "Part 2" should "find the score of the best recipe of 500 calories" in {
    Day15.part2(sampleIngredients) shouldBe 57600000
  }
}
