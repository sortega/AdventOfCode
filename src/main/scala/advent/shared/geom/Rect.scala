package advent.shared.geom

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

  def pointSet: Set[Point] =
    (for {
      x <- minX to maxX
      y <- minY to maxY
    } yield Point(x, y)).toSet

  def corners: Set[Point] =
    (for {
      x <- List(minX, maxX)
      y <- List(minY, maxY)
    } yield Point(x, y)).toSet
}
