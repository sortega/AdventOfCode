package advent.y2018

import scalaz.Scalaz._

import advent.shared.Time.timed
import advent.shared.geom.{Point, Rect}

object Day10 {
  final case class FlyingStar(pos: Point, velocity: Point) {
    def step: FlyingStar = copy(pos = pos + velocity)
  }

  final case class Constellation(stars: List[FlyingStar]) {
    def step: Constellation = Constellation(stars.map(_.step))

    val positions = stars.map(_.pos).toSet

    val rect: Rect = {
      val Some((minX, maxX)) = positions.map(_.x).extrema
      val Some((minY, maxY)) = positions.map(_.y).extrema
      Rect(minX, minY, maxX, maxY)
    }

    def size: Long =
      (rect.maxX.toLong - rect.minY.toLong + 1) * (rect.maxY.toLong - rect.minY.toLong + 1)

    override def toString: String = {
      if (size > 2000) "too large to display"
      else {
        val buffer = new StringBuilder
        for (y <- rect.minY to rect.maxY) {
          for (x <- rect.minX to rect.maxX) {
            buffer += (if (positions(Point(x, y))) '#' else '.')
          }
          buffer += '\n'
        }
        buffer.toString
      }
    }
  }

  object Constellation {
    private val Pattern = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

    def parse(lines: List[String]): Constellation =
      Constellation(lines.collect {
        case Pattern(x, y, dx, dy) => FlyingStar(Point(x.toInt, y.toInt), Point(dx.toInt, dy.toInt))
      })
  }

  def solve(constellation: Constellation, textHeight: Int = 10): Unit = {
    Stream
      .iterate(constellation)(_.step)
      .zipWithIndex
      .find {
        case (cons, _) => (cons.rect.maxY.toLong - cons.rect.minY.toLong + 1L) == textHeight
      }
      .foreach {
        case (cons, i) =>
          println(s"Step $i\n\n$cons\n")
      }
  }

  def main(args: Array[String]): Unit = {
    val input = Constellation.parse(inputResource(day = 10).getLines().toList)
    timed(solve(input))
  }
}
