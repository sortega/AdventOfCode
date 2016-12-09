package advent.y2016

case class Point(x: Int, y: Int) {
  def +(other: Point): Point = Point(x + other.x, y + other.y)
  def *(other: Point): Point = Point(x * other.x - y * other.y, x * other.y + y * other.x)
  def turnRight: Point = this * Point(x = 0, y = -1)
  def turnLeft: Point = this * Point(x = 0, y = 1)
  def norm1: Int = x.abs + y.abs
  def scaleBy(factor: Int): Point = Point(x * factor, y * factor)
}

object Point {
  val Origin = Point(x = 0, y = 0)
}
