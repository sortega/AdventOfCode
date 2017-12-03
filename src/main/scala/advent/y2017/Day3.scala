package advent.y2017

import scala.collection.mutable

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

  def part1(input: Int): Int = coordsFor(input).norm1

  def part2(input: Int): Int = {
    val written = mutable.Map(Point.Origin -> 1)
    Stream.from(2).foreach { i =>
      val pos   = coordsFor(i)
      val value = pos.adjacent8.toList.flatMap(written.get).sum
      written.put(pos, value)
      if (value > input) {
        return value
      }
    }
    throw new IllegalStateException("unreachable line")
  }

  def coordsFor(index: Int): Point =
    if (index <= 1) Point.Origin
    else {
      val radius           = radiusOfConcentricSquareFor(index)
      val offset           = offsetWithinSquare(radius, index)
      val (arm, armOffset) = locate(radius, offset)
      toPoint(radius, arm, armOffset)
    }

  private def radiusOfConcentricSquareFor(index: Int): Int =
    Math.ceil((Math.sqrt(index) + 1) / 2).toInt

  private def offsetWithinSquare(radius: Int, index: Int): Int = {
    val side = 2 * radius - 3
    index - side * side - 1
  }

  private def locate(radius: Int, offset: Int): (SpiralArm, Int) = {
    val armLength = 2 * radius - 2
    val armIndex  = offset / armLength
    val arm = armIndex match {
      case 0 => SpiralArm.Up
      case 1 => SpiralArm.Left
      case 2 => SpiralArm.Down
      case 3 => SpiralArm.Right
    }
    (arm, offset % armLength)
  }

  private def toPoint(radius: Int, arm: SpiralArm, offset: Int): Point = {
    val upArmStart = Point(x = radius - 1, y = 2 - radius)
    val armStart = arm match {
      case SpiralArm.Up    => upArmStart
      case SpiralArm.Left  => upArmStart.turnLeft
      case SpiralArm.Down  => upArmStart.turnLeft.turnLeft
      case SpiralArm.Right => upArmStart.turnRight
    }
    val path = arm.dir.scaleBy(offset)
    armStart + path
  }

  def main(args: Array[String]): Unit = {
    val input = 325489
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
