package advent.y2018

import advent.shared.geom.Point
import advent.y2018.Day11.Grid
import org.scalatest.{FlatSpec, Matchers}

final class Day11Test extends FlatSpec with Matchers {

  private val grid18 = Grid(serialNumber = 18)
  private val smallGrid = Grid(
    List(List(-3, 4, 2, 2, 2),
         List(-4, 4, 3, 3, 4),
         List(-5, 3, 3, 4, -4),
         List(4, 3, 3, 4, -3),
         List(3, 3, 3, -5, -1)))

  "The power calculation" should "work" in {
    Grid.cellPower(serialNumber = 8, at = Point(3, 5)) shouldBe 4
    Grid.cellPower(serialNumber = 57, at = Point(122, 79)) shouldBe -5
    Grid.cellPower(serialNumber = 39, at = Point(217, 196)) shouldBe 0
    Grid.cellPower(serialNumber = 71, at = Point(101, 153)) shouldBe 4
  }

  "A grid" should "compute window power" in {
    smallGrid.powerIn(Point(1, 1), size = 5) shouldBe smallGrid.power.flatten.sum
    grid18.powerIn(Point(1, 1), size = 300) shouldBe grid18.power.flatten.sum
  }

  "Part 1" should "find the highest 3x3 power area" in {
    Day11.part1(grid18) shouldBe Point(33, 45)
  }

  "Part 2" should "find the highest power area" in {
    Day11.part2(grid18) shouldBe (Point(90, 269), 16)
  }
}
