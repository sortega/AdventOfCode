package advent.y2017

import scala.annotation.tailrec

import advent.shared.Point
import advent.shared.Time.timed

object Day11 {
  sealed abstract class Dir {
    def move(pos: Point): Point
  }

  object Dir {
    case object N extends Dir {
      override def move(pos: Point): Point = pos + Point(x = 0, y = 1)
    }
    case object NE extends Dir {
      override def move(pos: Point): Point =
        pos + Point(x = 1, y = if (even(pos.x)) 1 else 0)
    }
    case object SE extends Dir {
      override def move(pos: Point): Point =
        pos + Point(x = 1, y = if (even(pos.x)) 0 else -1)
    }
    case object S extends Dir {
      override def move(pos: Point): Point = pos + Point(x = 0, y = -1)
    }
    case object SW extends Dir {
      override def move(pos: Point): Point =
        pos + Point(x = -1, y = if (even(pos.x)) 0 else -1)
    }
    case object NW extends Dir {
      override def move(pos: Point): Point =
        pos + Point(x = -1, y = if (even(pos.x)) 1 else 0)
    }

    val All = Set(N, NE, SE, S, SW, NW)

    def parse(input: String): Dir         = All.find(_.toString.equalsIgnoreCase(input)).get
    def parseAll(input: String): Seq[Dir] = input.split(",").map(parse).toSeq

    private def even(n: Int): Boolean = n % 2 == 0
  }

  def part1(input: String): Int =
    hexDistToOrigin(Dir.parseAll(input).foldLeft(Point.Origin) { (pos, dir) =>
      dir.move(pos)
    })

  private def hexDistToOrigin(pos: Point): Int = {
    @tailrec
    def greedyDescent(pos: Point, dist: Int = 0): Int =
      if (pos == Point.Origin) dist
      else
        greedyDescent(pos = Dir.All.map(_.move(pos)).minBy(_.euclideanDistanceTo(Point.Origin)),
                      dist = dist + 1)

    greedyDescent(pos)
  }

  def part2(input: String): Int =
    Dir
      .parseAll(input)
      .scanLeft(Point.Origin) { (pos, dir) =>
        dir.move(pos)
      }
      .map(hexDistToOrigin)
      .max

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 11).getLines().next()
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
