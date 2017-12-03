package advent.y2016

import advent.shared.Point
import advent.shared.Time.timed

object Day24 {

  type Route = Vector[Int]

  case class Maze(spots: Map[Int, Point], openSpaces: Set[Point]) {
    def distance(from: Point, to: Point): Int = {
      val path = new AStar[Point](distance = (_, _) => 1,
                                  neighbors = _.adjacent4.intersect(openSpaces),
                                  heuristic = point => (point - to).norm1).search(from, _ == to)
      path.getOrElse(throw new Error(s"No path from $from to $to")).size - 1
    }

    def spotDistances: Map[(Int, Int), Int] =
      for {
        (from, fromPos) <- spots
        (to, toPos) <- spots if to <= from
        key <- Seq((from, to), (to, from))
      } yield key -> distance(fromPos, toPos)
  }

  object Maze {
    def parse(input: String): Maze = {
      val charMap = (for {
        (line, y) <- input.lines.zipWithIndex
        (char, x) <- line.zipWithIndex
      } yield Point(x, y) -> char).toMap

      val spots = charMap.collect {
        case (point, number) if ('0' to '9') contains number =>
          number.toString.toInt -> point
      }

      val openSpaces = charMap.collect {
        case (point, char) if char != '#' && !char.isSpaceChar => point
      }.toSet

      Maze(spots, openSpaces)
    }
  }

  def shortestRouteDistance(distances: Map[(Int, Int), Int], routes: Int => Seq[Route]): Int = {
    def totalDistance(route: Route): Int =
      route
        .sliding(size = 2, step = 1)
        .map {
          case Vector(from, to) => distances(from, to)
        }
        .sum

    val spotsNum = distances.keys.map(_._1).max
    routes(spotsNum).map(totalDistance).min
  }

  private def openRoutes(n: Int): Seq[Vector[Int]] =
    (1 to n).toVector.permutations.map(route => 0 +: route).toSeq

  private def closedRoutes(n: Int): Seq[Vector[Int]] = openRoutes(n).map(_ :+ 0)

  def part1(input: String): Int =
    shortestRouteDistance(Maze.parse(input).spotDistances, openRoutes)

  def part2(input: String): Int =
    shortestRouteDistance(Maze.parse(input).spotDistances, closedRoutes)

  def main(args: Array[String]): Unit = {
    val input = dailyResource(24).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
