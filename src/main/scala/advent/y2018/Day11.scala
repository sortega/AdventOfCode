package advent.y2018

import scalaz.Memo

import advent.shared.Time.timed
import advent.shared.geom.Point

object Day11 {

  final case class Grid(power: List[List[Int]]) {
    private lazy val windowPower: ((Int, Int, Int)) => Int =
      Memo.immutableHashMapMemo[(Int, Int, Int), Int] {
        case (_, _, 0) => 0

        case (x, y, 1) => power(x)(y)

        case (x, y, size) =>
          val topLeft     = windowPower(x, y, 1)
          val bottomRight = windowPower(x + size - 1, y + size - 1, 1)
          val topRight    = windowPower(x + 1, y, size - 1)
          val bottomLeft  = windowPower(x, y + 1, size - 1)
          val center      = windowPower(x + 1, y + 1, size - 2)
          topLeft + bottomRight + topRight + bottomLeft - center
      }

    def powerIn(corner: Point, size: Int): Int = windowPower(corner.x - 1, corner.y - 1, size)
  }

  object Grid {
    def apply(serialNumber: Int): Grid =
      Grid(List.tabulate(300, 300) { (i, j) =>
        cellPower(serialNumber, at = Point(x = i + 1, y = j + 1))
      })

    def cellPower(serialNumber: Int, at: Point): Int = {
      val rackId       = at.x + 10
      val initialPower = rackId * at.y
      val power2       = initialPower + serialNumber
      val power3       = power2 * rackId
      val digit        = hundredsDigit(power3)
      digit - 5
    }

    private def hundredsDigit(n: Int): Int = n / 100 % 10
  }

  private def findHighestPoweredWindow(grid: Grid, sizeRange: Range.Inclusive) =
    (for {
      size <- sizeRange
      x    <- 1 to (300 - size + 1)
      y    <- 1 to (300 - size + 1)
      point = Point(x, y)
    } yield (point, size) -> grid.powerIn(point, size))
      .maxBy(_._2)
      ._1

  def part1(grid: Grid): Point = findHighestPoweredWindow(grid, sizeRange = 3 to 3)._1

  def part2(grid: Grid): (Point, Int) = findHighestPoweredWindow(grid, sizeRange = 1 to 300)

  def main(args: Array[String]): Unit = {
    val serialNumber = 5093
    val grid         = Grid(serialNumber)
    timed(println("Part 1 result: " + part1(grid)))
    timed(println("Part 2 result: " + part2(grid)))
  }
}
