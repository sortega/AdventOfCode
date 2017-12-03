package advent.y2016

import scala.annotation.tailrec

import advent.geom.Point

object Day1 {

  sealed trait Instruction
  object Instruction {
    case object TurnLeft extends Instruction
    case object TurnRight extends Instruction
    case class AdvanceBy(distance: Int) extends Instruction

    private val Pattern = """(R|L)(\d+)""".r

    def parse(input: String): Seq[Instruction] =
      Pattern
        .findAllMatchIn(input)
        .flatMap { m =>
          Seq(parseTurn(m.group(1)), parseAdvance(m.group(2)))
        }
        .toSeq

    private def parseTurn(input: String) = input match {
      case "R" => TurnRight
      case "L" => TurnLeft
    }

    private def parseAdvance(group: String) = AdvanceBy(group.toInt)
  }

  case class Context(position: Point, facing: Point, visited: Seq[Point] = Seq.empty) {
    def moveBy(instruction: Instruction): Context = instruction match {
      case Instruction.TurnLeft => copy(facing = facing.turnLeft)
      case Instruction.TurnRight => copy(facing = facing.turnRight)
      case Instruction.AdvanceBy(distance) =>
        val newlyVisited = (1 to distance).map(d => position + facing.scaleBy(d))
        copy(position = position + facing.scaleBy(distance),
             visited = visited ++ newlyVisited)
    }
  }
  object Context {
    val Initial = Context(position = Point.Origin, facing = Point(x = 0, y = 1))
  }

  def part1(input: String): Int =
    Instruction.parse(input).foldLeft(Context.Initial)(_ moveBy _).position.norm1

  @tailrec
  private def firstRepeated(positions: Seq[Point], seen: Set[Point] = Set.empty): Point =
    if (seen.contains(positions.head)) positions.head
    else firstRepeated(positions.tail, seen + positions.head)

  def part2(input: String): Int = {
    val visited = Instruction.parse(input).foldLeft(Context.Initial)(_ moveBy _).visited
    firstRepeated(visited).norm1
  }

  def main(args: Array[String]): Unit = {
    val input =
      "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3, L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5, R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
