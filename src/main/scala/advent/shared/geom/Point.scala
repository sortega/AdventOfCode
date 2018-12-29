package advent.shared.geom

final case class Point(x: Int, y: Int) {
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

  def euclideanDistanceTo(other: Point): Double = {
    val dx = x - other.x
    val dy = y - other.y
    scala.math.sqrt(dx * dx + dy * dy)
  }

  def manhattanDistanceTo(other: Point): Int = (x - other.x).abs + (y - other.y).abs
}

object Point {
  val Origin         = Point(x = 0, y = 0)
  val Up             = CardinalPoint.North.delta
  val Down           = CardinalPoint.South.delta
  val Left           = CardinalPoint.West.delta
  val Right          = CardinalPoint.East.delta
  val AdjacentDeltas = Set(Up, Down, Left, Right)
  val AdjacentDeltasWithDiagonals = (for {
    x <- -1 to 1
    y <- -1 to 1
    if x != 0 || y != 0
  } yield Point(x, y)).toSet

  def parse(string: String): Point = {
    val Array(x, y) = string.split(", ").map(_.toInt)
    Point(x, y)
  }
}
