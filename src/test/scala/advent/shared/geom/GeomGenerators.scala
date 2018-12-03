package advent.shared.geom
import org.scalacheck.{Arbitrary, Gen}

trait GeomGenerators {
  def pointGenerator(topLeft: Point, bottomRight: Point): Gen[Point] =
    for {
      x <- Gen.choose(topLeft.x, bottomRight.x)
      y <- Gen.choose(topLeft.y, bottomRight.y)
    } yield Point(x, y)

  implicit val arbitraryPoint: Arbitrary[Point] = Arbitrary(
    pointGenerator(Point(-10000, -10000), Point(10000, 10000)))
}

object GeomGenerators extends GeomGenerators
