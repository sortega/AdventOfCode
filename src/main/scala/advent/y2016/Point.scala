package advent.y2016

case class Point(x: Int, y: Int) {
  import Point._

  def +(other: Point): Point = Point(x + other.x, y + other.y)
  def -(other: Point): Point = Point(x - other.x, y - other.y)
  def *(other: Point): Point = Point(x * other.x - y * other.y, x * other.y + y * other.x)
  def turnRight: Point = this * Point(x = 0, y = -1)
  def turnLeft: Point = this * Point(x = 0, y = 1)
  def norm1: Int = x.abs + y.abs
  def scaleBy(factor: Int): Point = Point(x * factor, y * factor)
  def adjacent: Set[Point] = AdjacentDeltas.map(_ + this)
}

object Point {
  val Origin = Point(x = 0, y = 0)
  val AdjacentDeltas = Set(Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))
}
