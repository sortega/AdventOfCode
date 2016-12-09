package advent.y2015

import scalaz.Scalaz._
import scalaz._

object Day15 {

  case class Ingredient(properties: Map[String, Int]) {
    def *(factor: Int) = Ingredient(properties.mapValues(_ * factor))

    def +(other: Ingredient) = Ingredient(properties |+| other.properties)

    def score: Int = properties.filterKeys(_ != "calories")
      .values
      .map(_ max 0)
      .product

    def calories: Int = properties.getOrElse("calories", 0)
  }

  object Ingredient {
    private val IngredientPattern = """\w+: (.*)""".r
    private val PropertyPattern = """(\w+) (-?\d+)""".r

    def parse(input: String): Ingredient = {
      val IngredientPattern(properties) = input
      val propertiesMap = PropertyPattern.findAllMatchIn(properties)
        .map { propMatch =>
          propMatch.group(1) -> propMatch.group(2).toInt
        }
        .toMap
      Ingredient(propertiesMap)
    }
  }

  case class Recipe(ingredients: Seq[(Ingredient, Int)]) {
    private val equivalentIngredient = ingredients
      .map { case (ingredient, amount) => ingredient * amount }
      .reduce(_ + _)

    def totalScore: Int = equivalentIngredient.score
    def calories: Int = equivalentIngredient.calories
  }

  object Recipe {
    def allRecipes(teaSpoons: Int, ingredients: Seq[Ingredient]): Stream[Recipe] =
      allPartitions(teaSpoons, ingredients.size).map { allocation =>
        Recipe(ingredients.zip(allocation))
      }
  }

  def allPartitions(total: Int, buckets: Int): Stream[Vector[Int]] = (total, buckets) match {
    case (0, 0) => Stream(Vector.empty)
    case (0, _) => Stream(Vector.fill(buckets)(0))
    case (_, 0) => Stream.empty
    case _      => (0 to total).toStream.flatMap { taken =>
      allPartitions(total - taken, buckets - 1).map(taken +: _)
    }
  }

  def part1(input: String): Int = {
    val ingredients = input.trim.lines.map(Ingredient.parse).toSeq
    Recipe.allRecipes(teaSpoons = 100, ingredients).map(_.totalScore).max
  }

  def part2(input: String): Int = {
    val ingredients = input.trim.lines.map(Ingredient.parse).toSeq
    Recipe.allRecipes(teaSpoons = 100, ingredients)
      .filter(_.calories == 500)
      .map(_.totalScore)
      .max
  }

  def main(args: Array[String]): Unit = {
    val input = "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5\nCandy: capacity 0, durability 5, flavor -1, texture 0, calories 8\nButterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6\nSugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
