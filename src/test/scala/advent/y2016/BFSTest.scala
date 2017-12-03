package advent.y2016

import advent.geom.Point
import org.scalatest.{FlatSpec, Matchers}

class BFSTest extends FlatSpec with Matchers {

  "A BFS traversal" should "list closer paths before" in {
    BFS.traverse[Point](Point(0,0), _.adjacent4).take(6).toList shouldBe List(
      List(Point(0,0)),
      List(Point(0,0), Point(1,0)),
      List(Point(0,0), Point(0,1)),
      List(Point(0,0), Point(-1,0)),
      List(Point(0,0), Point(0,-1)),
      List(Point(0,0), Point(1,0), Point(2,0))
    )
  }

}
