package advent.y2018

import advent.shared.Time.timed
import advent.shared.geom.{Point, Rect}
import scalaz.Scalaz._

object Day6 {

  def part1(targets: List[Point]): Int = {
    val area             = enclosingRect(targets, padding = 10)
    val areasOfInfluence = closestTargetsInArea(targets, area)
    val areaSizes        = areaSizesByTarget(areasOfInfluence)
    val infiniteAreas    = targetsWithInfiniteAreas(area, areasOfInfluence)
    (areaSizes -- infiniteAreas).values.max
  }

  private def enclosingRect(points: List[Point], padding: Int = 0): Rect = {
    val Some((minX, maxX)) = points.map(_.x).extrema
    val Some((minY, maxY)) = points.map(_.y).extrema
    Rect(minX - padding, minY - padding, maxX + padding, maxY + padding)
  }

  private def closestTargetsInArea(targets: List[Point], area: Rect): Map[Point, Point] =
    (for {
      point  <- area.pointSet
      target <- closestTarget(targets, point)
    } yield point -> target).toMap

  private def closestTarget(targets: List[Point], point: Point): Option[Point] = {
    val distances               = targets.map(_.manhattanDistanceTo(point))
    val minDist                 = distances.min
    val indicesOfClosestTargets = distances.indices.filter(i => distances(i) == minDist)
    if (indicesOfClosestTargets.size == 1) Some(targets(indicesOfClosestTargets.head)) else None
  }

  private def targetsWithInfiniteAreas(area: Rect, influences: Map[Point, Point]): Set[Point] =
    (for {
      (point, target) <- influences
      if point.x == area.minX || point.x == area.maxX || point.y == area.minY || point.y == area.maxY
    } yield target).toSet

  private def areaSizesByTarget(finiteAreasOfInfluence: Map[Point, Point]): Map[Point, Int] =
    finiteAreasOfInfluence.values.groupBy(identity).mapValues(_.size)

  def part2(targets: List[Point], maxDist: Int = 10000): Int =
    enclosingRect(targets).pointSet.count { point =>
      totalDistanceTo(targets, point) < maxDist
    }

  private def totalDistanceTo(targets: List[Point], point: Point): Int =
    targets.map(_.manhattanDistanceTo(point)).sum

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 6).getLines().map(Point.parse).toList
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
