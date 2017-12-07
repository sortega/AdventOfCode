package advent.y2017

import scalaz.Scalaz._

import advent.shared.Time.timed
import fastparse.WhitespaceApi
import fastparse.all._

object Day7 {

  type Id = String

  case class ProgramSpec(id: Id, weight: Int, subPrograms: List[Id])

  case class Input(specs: List[ProgramSpec])

  object Input {
    private object Grammar {
      val White = WhitespaceApi.Wrapper(NoTrace(" ".rep))
      import White._

      val id: P[Id]                = P(CharIn('a' to 'z').rep(1).!)
      val weight: P[Int]           = P("(" ~/ CharIn('0' to '9').rep(1).! ~ ")").map(_.toInt)
      val subPrograms: P[List[Id]] = P("->" ~/ id.rep(min = 1, sep = "," ~/ Pass)).map(_.toList)
      val programSpec: P[ProgramSpec] = P(id ~ weight ~ subPrograms.? ~/ "\n").map {
        case (id, weight, maybeSubPrograms) =>
          ProgramSpec(id, weight, maybeSubPrograms.getOrElse(Nil))
      }
      val input: P[Input] = P(programSpec.rep ~ End).map(specs => Input(specs.toList))
    }

    def parse(text: String): Input = Grammar.input.parse(text).get.value
  }

  def part1(rawInput: String): String = findRoot(Input.parse(rawInput))

  private def findRoot(input: Input): Id = {
    val (ids, subprograms) = input.specs.foldMap { spec =>
      Set(spec.id) -> spec.subPrograms.toSet
    }
    (ids -- subprograms).head
  }

  case class ProgramTower(id: String, weight: Int, children: List[ProgramTower]) {
    lazy val totalWeight: Int = weight + children.map(_.totalWeight).sum

    def rebalance: Option[(Id, Int)] = {
      val groups = children.groupBy(_.totalWeight)
      if (groups.size <= 1) None
      else {
        val weights = groups.keySet
        groups
          .collectFirst {
            case (currentWeight, List(unbalancedTower)) =>
              val targetWeight = (weights - currentWeight).head
              unbalancedTower.rebalance.getOrElse(
                unbalancedTower.id -> (unbalancedTower.weight - currentWeight + targetWeight))
          }
      }
    }
  }

  object ProgramTower {
    def apply(input: Input): ProgramTower = {
      val specs = input.specs.map(spec => spec.id -> spec).toMap

      def towerFor(id: String): ProgramTower = {
        val spec = specs(id)
        ProgramTower(spec.id, spec.weight, spec.subPrograms.map(towerFor))
      }

      towerFor(findRoot(input))
    }
  }

  def part2(rawInput: String): Int = {
    val input = Input.parse(rawInput)
    val tower = ProgramTower(input)
    tower.rebalance.get._2
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 7).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
