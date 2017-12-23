package advent.y2017

import advent.shared.Time.timed
import advent.shared.geom.Point
import advent.shared.graphs.PointGraph

object Day14 {

  def part1(input: String): Int = decodeMaze(input).flatten.count(_ == 1)

  private val hexDigitToBinary: Map[Char, Vector[Int]] =
    (0 to 15).map { n =>
      val char = Integer.toHexString(n).head
      val bits = Vector.iterate(n, 4)(_ >> 1).map(_ & 1).reverse
      char -> bits
    }.toMap

  private def hexToBinary(hex: String): Vector[Int] =
    hex.flatMap(hexDigitToBinary).toVector

  private def decodeMaze(input: String): Vector[Vector[Int]] =
    Stream
      .from(0)
      .take(128)
      .toVector
      .map(index => hexToBinary(Day10.KnotHash(s"$input-$index")))

  private def freeSquares(maze: Vector[Vector[Int]]): Set[Point] =
    (for {
      (row, rowIndex)    <- maze.zipWithIndex
      (square, colIndex) <- row.zipWithIndex
      if square == 1
    } yield Point(y = rowIndex, x = colIndex)).toSet

  def part2(input: String): Int =
    PointGraph(freeSquares(decodeMaze(input))).connectedComponents.size

  def main(args: Array[String]): Unit = {
    val input = "nbysizxe"
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
