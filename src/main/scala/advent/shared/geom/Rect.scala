package advent.shared.geom

import scala.language.higherKinds
import scalaz.{Foldable, Functor}
import scalaz.Scalaz._

final case class Rect(minX: Int, minY: Int, maxX: Int, maxY: Int) {
  require(minX <= maxX)
  require(minY <= maxY)

  def contains(point: Point): Boolean =
    (point.x >= minX) && (point.x <= maxX) && (point.y >= minY) && (point.y <= maxY)

  def intersects(other: Rect): Option[Rect] = {
    val minX = this.minX max other.minX
    val maxX = this.maxX min other.maxX
    val minY = this.minY max other.minY
    val maxY = this.maxY min other.maxY
    if (minX <= maxX && minY <= maxY) Some(Rect(minX, minY, maxX, maxY))
    else None
  }

  def xRange: Range.Inclusive = minX to maxX
  def yRange: Range.Inclusive = minY to maxY

  def pointSet: Set[Point] =
    (for {
      x <- xRange
      y <- yRange
    } yield Point(x, y)).toSet

  def corners: Set[Point] =
    (for {
      x <- List(minX, maxX)
      y <- List(minY, maxY)
    } yield Point(x, y)).toSet
}

object Rect {
  def enclosing[F[_]: Functor: Foldable](points: F[Point]): Option[Rect] = for {
    (minX, maxX) <- points.map(_.x).extrema
    (minY, maxY) <- points.map(_.y).extrema
  } yield Rect(minX, minY, maxX, maxY)
}
