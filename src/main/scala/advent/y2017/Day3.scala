package advent.y2017

import scala.annotation.tailrec

import advent.geom.Point
import advent.y2016.timed

object Day3 {

  sealed abstract class SpiralArm(val dir: Point)

  object SpiralArm {
    case object Up    extends SpiralArm(Point.Up)
    case object Left  extends SpiralArm(Point.Left)
    case object Down  extends SpiralArm(Point.Down)
    case object Right extends SpiralArm(Point.Right)
  }

  def part1(input: Int): Int = spiralCoords(input - 1).norm1

  def part2(input: Int): Int = {
    @tailrec
    def iterate(spiral: Stream[Point], written: Map[Point, Int]): Int = {
      val pos   = spiral.head
      val value = pos.adjacent8.toList.flatMap(written.get).sum
      if (value > input) value
      else iterate(spiral.tail, written.updated(pos, value))
    }

    iterate(spiralCoords.tail, Map(Point.Origin -> 1))
  }

  def spiralCoords: Stream[Point] = {
    val radii = Stream.from(1)
    val deltas = radii.flatMap { radius =>
      val side = 2 * radius - 1
      Vector.fill(side - 2)(Point.Up) ++
        Vector.fill(side - 1)(Point.Left) ++
        Vector.fill(side - 1)(Point.Down) ++
        Vector.fill(side)(Point.Right)
    }
    deltas.scanLeft(Point.Origin)(_ + _)
  }

  def main(args: Array[String]): Unit = {
    val input = 325489
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
