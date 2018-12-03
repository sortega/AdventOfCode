package advent.y2018

import scala.util.matching.Regex

import advent.shared.Time.timed
import advent.shared.geom.{Point, Rect}

object Day3 {
  final case class Claim(id: Int, rect: Rect) {
    def pointSet: Set[Point]              = rect.pointSet
    def intersects(other: Claim): Boolean = this.rect.intersects(other.rect).nonEmpty
  }

  object Claim {
    val Pattern: Regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

    def parse(s: String): Claim = s match {
      case Pattern(id, x, y, w, h) =>
        Claim(id.toInt, Rect(x.toInt, y.toInt, x.toInt + w.toInt - 1, y.toInt + h.toInt - 1))
    }
  }

  def part1(claims: List[Claim]): Int =
    claims.flatMap(_.pointSet).groupBy(identity).values.count(_.size >= 2)

  def part2(claims: List[Claim]): Int = {
    val claimsWithCollisions =
      (for {
        i <- claims.indices
        j <- (i + 1) until claims.length
        if claims(i).intersects(claims(j))
      } yield Set(i, j)).flatten.toSet

    val withoutCollisions = claims.indices.filterNot(claimsWithCollisions).map(claims.apply)
    require(withoutCollisions.size == 1)
    withoutCollisions.head.id
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 3).getLines().map(Claim.parse).toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
