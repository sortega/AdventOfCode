package advent.y2017

import advent.shared.Time.timed

object Day20 {

  case class Point3D(x: BigInt, y: BigInt, z: BigInt) {
    def +(other: Point3D): Point3D       = Point3D(x + other.x, y + other.y, z + other.z)
    def -(other: Point3D): Point3D       = Point3D(x - other.x, y - other.y, z - other.z)
    def norm1: BigInt                    = x.abs + y.abs + z.abs
    def scaleBy(factor: BigInt): Point3D = Point3D(x * factor, y * factor, z * factor)
  }

  case class Particle(p: Point3D, v: Point3D, a: Point3D) {
    def tick: Particle            = copy(p = p + v + a, v = v + a)
    def posAt(n: BigInt): Point3D = p + v.scaleBy(n) + a.scaleBy(n * (n + 1) / 2)
    def velAt(n: BigInt): Point3D = v + a.scaleBy(n)
  }

  object Particle {
    private val Pattern =
      """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r

    def parseAll(input: String): List[Particle] =
      Pattern
        .findAllMatchIn(input)
        .map { m =>
          Particle(
            parsePoint(m.subgroups.slice(0, 3)),
            parsePoint(m.subgroups.slice(3, 6)),
            parsePoint(m.subgroups.slice(6, 9))
          )
        }
        .toList

    def parsePoint(fields: List[String]): Point3D = {
      val List(x, y, z) = fields
      Point3D(x.toInt, y.toInt, z.toInt)
    }
  }

  def part1(input: String): Int = {
    val particles = Particle.parseAll(input)
    val slowest   = particles.minBy(p => (p.a.norm1, p.v.norm1, p.p.norm1))
    particles.indexOf(slowest)
  }

  def part2(input: String): Int = {
    val particles = Particle.parseAll(input)
    Stream
      .iterate(particles.toSet) { population =>
        population
          .groupBy(_.posAt(1))
          .collect {
            case (_, particlesAtPos) if particlesAtPos.size < 2 => particlesAtPos.head.tick
          }
          .toSet
      }
      .drop(10000) // Not really checking convergence...
      .head
      .size
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 20).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
