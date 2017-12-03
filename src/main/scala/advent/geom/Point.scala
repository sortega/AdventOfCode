package advent.geom

case class Point(x: Int, y: Int) {
  import Point._

  def +(other: Point): Point      = Point(x + other.x, y + other.y)
  def -(other: Point): Point      = Point(x - other.x, y - other.y)
  def *(other: Point): Point      = Point(x * other.x - y * other.y, x * other.y + y * other.x)
  def turnRight: Point            = this * Point(x = 0, y = -1)
  def turnLeft: Point             = this * Point(x = 0, y = 1)
  def norm1: Int                  = x.abs + y.abs
  def scaleBy(factor: Int): Point = Point(x * factor, y * factor)
  def adjacent4: Set[Point]       = AdjacentDeltas.map(_ + this)
  def adjacent8: Set[Point]       = AdjacentDeltasWithDiagonals.map(_ + this)
}

object Point {
  val Origin         = Point(x = 0, y = 0)
  val Up             = Point(0, 1)
  val Down           = Point(0, -1)
  val Left           = Point(-1, 0)
  val Right          = Point(1, 0)
  val AdjacentDeltas = Set(Up, Down, Left, Right)
  val AdjacentDeltasWithDiagonals = (for {
    x <- -1 to 1
    y <- -1 to 1
    if x != 0 || y != 0
  } yield Point(x, y)).toSet
}
