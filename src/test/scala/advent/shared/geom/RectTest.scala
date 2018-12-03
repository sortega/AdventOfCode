package advent.shared.geom

import advent.shared.test.UnitTest

class RectTest extends UnitTest {

  "A rect" should "intersect another" in {
    val r1 = Rect(minX = 1, minY = 1, maxX = 3, maxY = 3)
    val r2 = Rect(minX = 2, minY = 2, maxX = 2, maxY = 2)
    val r3 = Rect(minX = 10, minY = 10, maxX = 12, maxY = 12)
    val r4 = Rect(minX = 2, minY = -10, maxX = 2, maxY = 20)

    r1.intersects(r2) should ===(Some(r2))
    r2.intersects(r1) should ===(Some(r2))

    r1.intersects(r3) shouldBe empty

    r1.intersects(r4) should ===(Some(Rect(2, 1, 2, 3)))
  }
}
