package advent.y2017

import scala.annotation.tailrec

import advent.shared.Point
import advent.shared.Time.timed

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

  // TODO: a previous day was also finding all connected components. This can be abstracted away
  private def connectedComponents(nodes: Set[Point]): List[Set[Point]] = {
    @tailrec
    def expandComponent(open: List[Point], closed: Set[Point]): Set[Point] = open match {
      case Nil => closed
      case node :: remainingOpen if nodes.contains(node) && !closed.contains(node) =>
        expandComponent(remainingOpen ++ node.adjacent4, closed + node)
      case _ :: remainingOpen => expandComponent(remainingOpen, closed)
    }

    def componentContaining(node: Point): Set[Point] =
      expandComponent(open = List(node), closed = Set.empty)

    @tailrec
    def allConnectedComponents(unassignedNodes: Set[Point],
                               components: List[Set[Point]] = Nil): List[Set[Point]] =
      if (unassignedNodes.isEmpty) components
      else {
        val component = componentContaining(unassignedNodes.head)
        allConnectedComponents(unassignedNodes.diff(component), component +: components)
      }

    allConnectedComponents(nodes)
  }

  def part2(input: String): Int = connectedComponents(freeSquares(decodeMaze(input))).size

  def main(args: Array[String]): Unit = {
    val input = "nbysizxe"
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
