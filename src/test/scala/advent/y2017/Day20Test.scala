package advent.y2017

import advent.y2017.Day20.{Particle, Point3D}
import org.scalatest.{FlatSpec, Matchers}

class Day20Test extends FlatSpec with Matchers {

  val p0 = Particle(p = Point3D(3, 0, 0), v = Point3D(2, 0, 0), a = Point3D(-1, 0, 0))
  val p1 = Particle(p = Point3D(4, 0, 0), v = Point3D(0, 0, 0), a = Point3D(-2, 0, 0))
  val p172 = Particle(Point3D(-197, -954, -115), Point3D(20, 85, 11), Point3D(0, -1, 0))
  val p300 = Particle(Point3D(1585, 1025, -980), Point3D(-56, -35, 35), Point3D(0, 0, 0))

  "Part 1" should "tell which particle will be closest in the long term" in {
    Day20.part1("""p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
                  |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
                  |""".stripMargin) shouldBe 0
  }

  "Part 2" should "count the number of particles that doesn't collide" in {
    Day20.part2("""p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
                  |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
                  |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
                  |p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>
                  |""".stripMargin) shouldBe 1
  }
}
