package advent

import advent.Day14.Reindeer
import org.scalatest.{FlatSpec, ShouldMatchers}

class Day14Test extends FlatSpec with ShouldMatchers {

  "A reindeer spec" should "be parsed from string" in {
    Reindeer.parse(
      "Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.") shouldBe
      Reindeer(speed = 22, flyTime = 8, restTime = 165)
  }

  it should "provide the distance as time goes by" in {
    val rudolf = Reindeer(speed = 10, flyTime = 5, restTime = 5)
    rudolf.distances.take(20) shouldBe Seq(10, 20, 30, 40,  50,  50,  50,  50,  50,  50,
                                           60, 70, 80, 90, 100, 100, 100, 100, 100, 100)
  }

  val sampleRace =
    """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
      |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
    """.stripMargin

  "Part 1" should "compute the distance covered by the winner" in {
    Day14.part1(sampleRace, 1000) shouldBe 1120
  }

  "Part 2" should "compute the points awarded by the winner" in {
    Day14.part2(sampleRace, 1000) shouldBe 689
  }
}
