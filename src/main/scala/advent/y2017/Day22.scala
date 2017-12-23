package advent.y2017

import advent.shared.Time.timed
import advent.shared.geom.{CardinalPoint, Point}
import advent.shared.geom.CardinalPoint._

object Day22 {

  case class Carrier(pos: Point, dir: CardinalPoint) {
    def advance: Carrier   = copy(pos = pos + dir.delta)
    def turnRight: Carrier = copy(dir = dir.turnRight)
    def turnLeft: Carrier  = copy(dir = dir.turnLeft)
  }

  object Carrier {
    val Initial = Carrier(pos = Point.Origin, dir = North)
  }

  trait World {
    def bursts: Stream[World] = Stream.iterate(this)(_.burst)
    def burst: World
    def shouldInfect: Boolean
  }

  case class SimpleWorld(carrier: Carrier, infected: Set[Point]) extends World {

    override def burst: SimpleWorld = turnCarrier.flipCell(carrier.pos).advance

    override def shouldInfect: Boolean = !infected(carrier.pos)

    def toComplex: ComplexWorld =
      ComplexWorld(carrier, infected.map(cell => cell -> Infected).toMap)

    private def turnCarrier: SimpleWorld =
      copy(carrier = if (infected(carrier.pos)) carrier.turnRight else carrier.turnLeft)

    private def flipCell(cell: Point): SimpleWorld =
      copy(infected = if (infected(cell)) infected - cell else infected + cell)

    private def advance: SimpleWorld = copy(carrier = carrier.advance)
  }

  object SimpleWorld {
    def parse(input: String): SimpleWorld = {
      val lines = input.lines.toList
      val rows  = lines.size
      val cols  = lines.head.length
      val infected = for {
        (row, rowIndex)  <- lines.zipWithIndex
        (char, colIndex) <- row.zipWithIndex
        if char == '#'
      } yield Point(x = colIndex - cols / 2, y = rows / 2 - rowIndex)
      SimpleWorld(Carrier.Initial, infected.toSet)
    }
  }

  sealed trait Cell
  case object Clean    extends Cell
  case object Weakened extends Cell
  case object Infected extends Cell
  case object Flagged  extends Cell

  case class ComplexWorld(carrier: Carrier, cells: Map[Point, Cell]) extends World {
    override def burst: ComplexWorld = turnCarrier.flipCell(carrier.pos).advance

    override def shouldInfect: Boolean = cells.get(carrier.pos).contains(Weakened)

    private def turnCarrier: ComplexWorld =
      copy(carrier = cells.getOrElse(carrier.pos, Clean) match {
        case Clean    => carrier.turnLeft
        case Weakened => carrier
        case Infected => carrier.turnRight
        case Flagged  => carrier.turnRight.turnRight
      })

    private def flipCell(cell: Point): ComplexWorld = {
      val nextState = cells.getOrElse(carrier.pos, Clean) match {
        case Clean    => Weakened
        case Weakened => Infected
        case Infected => Flagged
        case Flagged  => Clean
      }
      copy(
        cells = if (nextState == Clean) cells - carrier.pos else cells + (carrier.pos -> nextState))
    }

    private def advance: ComplexWorld = copy(carrier = carrier.advance)
  }

  private def countInfections(world: World, bursts: Int) =
    world.bursts.take(bursts).foldLeft(0) { (count, state) =>
      count + (if (state.shouldInfect) 1 else 0)
    }

  def part1(input: String): Int = countInfections(SimpleWorld.parse(input), 10000)

  def part2(input: String): Int = countInfections(SimpleWorld.parse(input).toComplex, 10000000)

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 22).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
