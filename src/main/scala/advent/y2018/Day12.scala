package advent.y2018

import advent.shared.Time.timed
import scalaz.Scalaz._

object Day12 {
  final case class Generation(plants: Set[Int]) extends AnyVal {
    def step(ruleSet: RuleSet): Generation = {
      val contexts: Map[Int, Set[Int]] = plants.foldMap { index =>
        (-2 to 2).map(delta => index - delta -> Set(delta)).toMap
      }
      Generation(contexts.collect {
        case (index, context) if ruleSet.producingContexts.contains(context) => index
      }.toSet)
    }

    def steps(ruleSet: RuleSet): Stream[Generation] = Stream.iterate(this)(_.step(ruleSet))

    override def toString: String = {
      val Some((min, max)) = plants.extrema
      (min to max).map(index => if (plants(index)) '#' else '.').mkString(s"from $min: ", "", "")
    }
  }

  object Generation {
    def parse(line: String): Generation = Generation(
      line
        .dropWhile(c => c != '.' && c != '#')
        .zipWithIndex
        .collect {
          case ('#', index) => index
        }
        .toSet
    )
  }

  final case class RuleSet(producingContexts: Set[Set[Int]]) extends AnyVal

  object RuleSet {
    val Pattern = """(.....) => #""".r

    def parse(lines: List[String]): RuleSet = RuleSet(
      lines.collect {
        case Pattern(context) =>
          context.zipWithIndex.collect {
            case ('#', index) => index - 2
          }.toSet
      }.toSet
    )
  }

  def part1(ruleSet: RuleSet, generation: Generation): Int = {
    val targetGeneration = generation.steps(ruleSet).drop(20).head
    targetGeneration.plants.sum
  }

  def part2(ruleSet: RuleSet, generation: Generation): Long = {
    val generations = generation.steps(ruleSet)
    val steadyStateIndex = 500
    val steadyState = generations(steadyStateIndex)
    val steadyStateSum = steadyState.plants.sum
    val steadyStateNum = steadyState.plants.size
    val targetGeneration = 50000000000L
    steadyStateSum + (targetGeneration - steadyStateIndex) * steadyStateNum
  }

  def main(args: Array[String]): Unit = {
    val input      = inputResource(day = 12).getLines().toList
    val generation = Generation.parse(input.head)
    val ruleSet    = RuleSet.parse(input.tail)
    timed(println("Part 1 result: " + part1(ruleSet, generation)))
    timed(println("Part 2 result: " + part2(ruleSet, generation)))
  }
}
